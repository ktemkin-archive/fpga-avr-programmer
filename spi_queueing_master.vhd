--  
-- AVR-910 Compliant In-System Programmer for Soft-IP AVR-compatible microcontrollers.
-- Queuing SPI transmitter.
--
-- The MIT License (MIT)
-- 
-- Copyright (c) 2014 Kyle J. Temkin
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
-- 
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library avr_programmer;
use avr_programmer.constructs.all;


--
-- Simple queueing transmitter. 
-- Allows transmission of simple byte sequences.
--
entity spi_queueing_master is
  generic(

    --The length of the transmit queue: this specifies the maximum amount of
    --bytes that can be queued for transmission.
    queue_length : integer := 5;
 
    --The clock polarity and phase of the relevant SPI transmission.
    --Together, these bits are sometimes called the SPI mode.
    clock_polarity : std_ulogic := '0';
    clock_phase    : std_ulogic := '0'

  );
  port(
    --Global signals.
    clk   : in std_ulogic;
    reset : in std_ulogic := '0';

    --
    -- Data input and relevant control signals.
    --

    --The data to be queued; this should be a collection of bytes queue_length
    --long. If you'd like to transmit less than the queue length, the extra bytes
    --will act as Don't Cares.
    data_to_queue     : in byte_vector(0 to queue_length - 1);

    --The number of bytes to transmit.
    data_length       : in integer range 0 to queue_length;

    --Request that our SPI controller enqueue new data.
    request_enqueue  : in std_ulogic;

    --
    -- Data output and relevant controls.
    -- 

    --The receive control signal. This should be one if the SPI controller
    --is to recieve; this ensures that the transaction does not cease until
    --after we've stopped receiving. If we're transmitting, this value 
    --doesn't matter, as every transmit is also implicitly a receive.
    receive            : in std_ulogic;

    --The most recently received byte.
    last_received_byte : out byte;

    --Strobe signal which goes high for a single cycle when new data has been
    --received.
    new_byte_received  : out std_ulogic;

    --
    -- Status flags.
    --

    --Strobe. Indicates that the SPI controller is ready for a new set of data
    --as part of the /current/ packet. If new data is enqueued on the
    --next clock cycle, it will be considered part of the same transmission.
    ready_for_continuous_enqueue  : out std_ulogic;

    --Strobes. ndicates that the SPI controller is ready for a new set of data to
    --transmit. If this follows a prior packet, the data will be transmitted 
    --as a new packet, as the chip-select has already been deasserted.
    ready_for_new_packet_enqueue : out std_ulogic; 

    --Strobe. Indicates that the transmission has completed, as of this clock cycle.
    transmit_complete : buffer std_ulogic;

    --Indicates that we're ready for a new set of data to be transmitted.
    --This will go high at the same time as ready_for_new_packet_enqueue,
    --but will remain high afterwards until request_enqueue is asserted.
    ready_for_enqueue : out std_ulogic;

    --
    -- Core SPI communications bus.
    --
    flash_sck      : buffer std_ulogic;
    flash_miso     : in     std_ulogic;
    flash_mosi     : buffer std_ulogic;
    flash_cs       : buffer std_ulogic
  );
end spi_queueing_master;

architecture Behavioral of spi_queueing_master is

  --SPI control and status flags.
  --See the spi_master for documentation of these signals.
  signal core_is_busy, core_was_busy, core_data_accepted, core_data_complete : std_ulogic;
  signal initiate_transaction, continue_transaction : std_ulogic := '0';

  --
  --SPI command "queue".
  --

  --Stores a queue of bytes to be transmitted, in a shift-out configuration.
  --The least significant position in the queue represents the next byte to be transmitted.
  signal transmit_queue : byte_vector(0 to data_to_queue'high);

  --Represents the number of bytes remaining in the currently queued transmission.
  --Note that this need not be positive for a transmission to continue.
  signal bytes_remaining : integer range 0 to data_to_queue'high;

  --Indicates that we're current waiting for a new set of enqueued data.
  signal waiting_for_enqueue : std_ulogic;


begin

  --
  -- The main SPI controller for the given SPI flash.
  --
  SPI_CONTROLLER:
  entity work.spi_master 
    generic map(
      --Set the number of parallel devices connected to this SPI bus.
      slaves  => 1,

      --... and set the SPI communications word size.
      d_width => byte'length
    )
    port map(
      clock   => clk,

      -- Control signals.
      reset_n => not reset,
      enable  => initiate_transaction,

      --SPI signals
      sclk    => flash_sck,
      ss_n(0) => flash_cs,
      miso    => flash_miso,
      mosi    => flash_mosi,

      --Always talk to the first device, which is at address 0.
      addr    => 0,

      --And set the fastest SPI clock rate possible.
      clk_div => 32,

      --Clock polarity and phase.
      cpol    => '0',
      cpha    => '0',

      --Parallel data in and out of the SPI master.
      tx_data => transmit_queue(0),
      rx_data => last_received_byte,

      --Status flags.
      busy    => core_is_busy,
      cont    => continue_transaction
    );

    
  --
  -- Rising edge detect for the SPI "busy" signal. 
  -- Used to determine when to move to the next instruction.
  --
  core_was_busy      <= core_is_busy when rising_edge(clk);
  core_data_accepted <= core_is_busy and not core_was_busy;
  core_data_complete <= not core_is_busy and core_was_busy;

  --
  -- Simple queueing transmitter: allows sequences of bytes to be transmitted via SPI.
  -- 
  QUEUEING_TRANSMITTER:
  process(clk)
  begin
    if rising_edge(clk) then

      --If the main state machine is requesting that we enqueue
      --a new set of data, do so.
      if request_enqueue = '1' then
        
        --Start a new transaction.
        initiate_transaction <= '1'; 

        --Indicate that we're no longer ready to have a new packet enqueued.
        waiting_for_enqueue <= '0';

        --Populate the queue...
        transmit_queue  <= data_to_queue;
        bytes_remaining <= data_length;


      --Otherwise, if the SPI transmtter subsystem has accepted our data...
      elsif core_data_accepted = '1' then

        --...move on to transmitting the next byte...
        transmit_queue <= transmit_queue(1 to transmit_queue'high) & x"00";

        --... mark this as "no longer" an initiation.
        initiate_transaction <= '0';

        --If data remains, decrease the number of bytes to be transmitted.
        if bytes_remaining > 0 then
          bytes_remaining <= bytes_remaining - 1;
        end if;

      elsif core_data_complete = '1' and bytes_remaining = 0 then
        waiting_for_enqueue <= '1';
      end if;

    end if;
  end process;

  --Signals which indicates when a queued transmission has completed. 
  ready_for_continuous_enqueue <= '1' when bytes_remaining = 1 and core_data_accepted = '1' else '0';
  ready_for_new_packet_enqueue <= (transmit_complete or waiting_for_enqueue) and not request_enqueue;
  transmit_complete            <= '1' when bytes_remaining = 0 and core_data_complete = '1' else '0';

  --Signals which indicate when a recieve is complete.
  new_byte_received <= core_data_complete;

  --Continue the transaction for as long as we don't get to 0 bytes remaining. 
  --If the parent design wants to continue a transmission, they will requeue a transmission 
  --just as this becomes zero, forcing continue transaction to be high before the next time
  --the SPI master checks for a new byte.
  continue_transaction <= '1' when (bytes_remaining > 0) or receive = '1' else '0';



end Behavioral;

