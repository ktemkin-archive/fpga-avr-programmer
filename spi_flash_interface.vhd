--  
-- AVR-910 Compliant In-System Programmer for Soft-IP AVR-compatible microcontrollers.
-- Simple demonstration fixture.
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

entity spi_program_memory is 
  generic(
    
    --The address in the SPI flash at which AVR programs are stored.
    start_address : in std_ulogic_vector(23 downto 0) := x"050000"; 

    --The number of clock cycles to delay before starting an SPI
    --transaction, after a reset.
    startup_delay : in integer := 4095;

    --The size of the given memory, in kilobytes.
    memory_size   : in integer := 8

  );
  port(

    --Control signals: our system clock and global enable.
    clk      : in std_ulogic;
    enable   : in std_ulogic;  
    reset    : in std_ulogic := '0';

    --SPI communications lines.
    flash_sck      : buffer std_ulogic;
    flash_miso     : in     std_ulogic;
    flash_mosi     : buffer std_ulogic;
    flash_cs       : buffer std_ulogic;

    --Operation flags.
    write    : in std_ulogic;
    erase    : in std_ulogic;

    --The address to be affected.
    address : in std_ulogic_vector(15 downto 0);

    --TODO: Replace me with a word type?
    data_in  : in std_ulogic_vector(15 downto 0);
    data_out : out std_ulogic_vector(15 downto 0);

    --Status flags.
    bootstrap_complete : out std_ulogic := '0';

    --DEBUG
    transact_enable, busy  : out std_ulogic


  );
end spi_program_memory;


architecture spi_flash_buffered of spi_program_memory is
  
  --Signals that interconnect our SPI controller to our internal hardware.
  signal received_data : std_ulogic_vector(7 downto 0);

  --SPI control and status flags.
  signal spi_is_busy, spi_was_busy, spi_data_accepted, spi_data_complete : std_ulogic;
  signal initiate_transaction, continue_transaction : std_ulogic := '0';

  --
  --SPI command "queue".
  --
  type byte_queue is array(natural range <>) of std_ulogic_vector(7 downto 0);

  --Stores a queue of bytes to be transmitted, in a shift-out configuration.
  --The least significant position in the queue represents the next byte to be transmitted.
  signal spi_data_to_queue, spi_transmit_queue : byte_queue(0 to 4);

  --Represents the number of bytes remaining in the currently queued transmission.
  --Note that this need not be positive for a transmission to continue.
  signal spi_bytes_to_queue, spi_bytes_remaining : integer range 0 to spi_transmit_queue'high + 1;

  --Request that our SPI controller enqueue new data.
  signal spi_request_enqueue : std_ulogic;

  --Request that our contorller enqueue a new set of data.
  signal spi_queue_complete : std_ulogic;

  --Indicates that a queued transmit has completed; only valid once per queued transmit.
  --Once a queued transmit is complete, this signal will go high once per recieved byte.
  signal spi_queued_transmit_complete : std_ulogic;

  --
  --SPI command constants.
  --
  constant COMMAND_FAST_READ : std_ulogic_vector(7 downto 0) := x"0B";

  --The full command to schedule a fast read from the relevant starting address in the SPI flash.
  constant COMMAND_READ_FROM_START : byte_queue := (COMMAND_FAST_READ, start_address(23 downto 16), start_address(15 downto 8), start_address(7 downto 0), x"00");

  --Local memory data exchange signals.
  signal local_memory_address     : std_ulogic_vector(15 downto 0) := (others => '0');
  signal data_to_local_memory, data_from_local_memory : std_ulogic_vector(15 downto 0);
  signal write_to_local_memory, clear_local_memory, bootstrap_in_progress : std_ulogic;

  --Bootstrap copies of the local memory data exchange signals.
  signal bootstrap_memory_address : unsigned(15 downto 0)          := (others => '0');
  signal bootstrap_data_to_local_memory, bootstrap_data_from_local_memory : std_ulogic_vector(15 downto 0);
  signal bootstrap_write_to_local_memory : std_ulogic;

  --"Wait time" counter.
  signal cycles_waited : unsigned(11 downto 0);

  --Signals for the core FSM.
  type state_type is 
    (
      STARTUP,
      WAIT_FOR_INITIALIZATION,

      START_BOOTSTRAP_READ,
      BOOTSTRAP_SEND_READ_COMMAND,
      BOOTSTRAP_POPULATE_MEMORY_HIGH_BYTE,
      BOOTSTRAP_POPULATE_MEMORY_LOW_BYTE,
      BOOTSTRAP_INCREMENT_ADDRESS,

      IDLE
    );
  signal state : state_type := STARTUP;

begin

  -- TODO: Hook this up correctly once write operations are supported.
  data_out <= data_from_local_memory;

  --
  -- Local (fast) memory, which stores the program being executed.
  -- Unlike our SPI memory, this memory can read data in a single cycle.
  --
  LOCAL_MEMORY:
  entity work.program_memory(volatile)
    port map(
      clk      => clk,
      enable   => '1',

      write    => write_to_local_memory,
      --erase    => clear_local_memory,
      erase    => erase,

      address  => local_memory_address,
      data_in  => data_to_local_memory,
      data_out => data_from_local_memory
    );


  --
  -- Multi-channel multiplexer which selects the local memory inputs
  -- according to the current operating mode.
  --
  process(bootstrap_in_progress)
  begin

    --If we're in the boostrap mode, allow the bootstrap controller
    --to control the local memory.
    if bootstrap_in_progress = '1' then
      local_memory_address    <= std_ulogic_vector(bootstrap_memory_address);
      write_to_local_memory   <= bootstrap_write_to_local_memory;
      data_to_local_memory    <= bootstrap_data_to_local_memory;

    --Otherwise, allow the local memory to be accessed using the
    --external controls.
    else
      local_memory_address    <= address;
      write_to_local_memory   <= write;
      data_to_local_memory    <= data_in;
    end if;

  end process;


  --
  -- The main SPI controller for the given SPI flash.
  --
  SPI_CONTROLLER:
  entity work.spi_master 
    generic map(
      --Set the number of parallel devices connected to this SPI bus.
      slaves  => 1,

      --... and set the SPI communications word size.
      d_width => 8
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
      tx_data => spi_transmit_queue(0),
      rx_data => received_data,

      --Status flags.
      busy    => spi_is_busy,
      cont    => continue_transaction
    );

    
    --
    -- Rising edge detect for the SPI "busy" signal. 
    -- Used to determine when to move to the next instruction.
    --
    spi_was_busy      <= spi_is_busy when rising_edge(clk);
    spi_data_accepted <= spi_is_busy and not spi_was_busy;
    spi_data_complete <= not spi_is_busy and spi_was_busy;

    --
    -- Main controller flags.
    --
    bootstrap_complete <= not bootstrap_in_progress;

    --
    -- Simple queueing transmitter: allows sequences of bytes to be transmitted via SPI.
    -- 
    QUEUEING_TRANSMITTER:
    process(clk)
    begin
      if rising_edge(clk) then

        --If the main state machine is requesting that we enqueue
        --a new set of data, do so.
        if spi_request_enqueue = '1' then
          spi_transmit_queue  <= spi_data_to_queue;
          spi_bytes_remaining <= spi_bytes_to_queue;

        --Otherwise, if the SPI transmtter subsystem has accepted our data...
        elsif spi_data_accepted = '1' then

          --...move on to transmitting the next byte...
          spi_transmit_queue <= spi_transmit_queue(1 to spi_transmit_queue'high) & x"00";

          --... and decrease the number of bytes to be transmitted 
          if spi_bytes_remaining > 0 then
            spi_bytes_remaining <= spi_bytes_remaining - 1;
          else
             
          end if;

        end if;

      end if;
    end process;

    --Signal which indicates when a queued transmission has completed. See the disclaimer at the signal's definition.
    spi_queued_transmit_complete <= '1' when spi_bytes_remaining = 0 and spi_data_complete = '1' else '0';

    --
    -- Main controller FSM for the whole device.
    --
    CONTROLLER:
    process(clk)
    begin
      if rising_edge(clk) then

        if reset = '1' then
          state <= STARTUP;
        else

          --Keep the following signals low unless explicitly assertd.
          initiate_transaction            <= '0';
          continue_transaction            <= '0';
          bootstrap_write_to_local_memory <= '0';
          clear_local_memory              <= '0';
          spi_request_enqueue             <= '0';

          case state is

            --
            -- Startup (initialization) state.
            --
            when STARTUP =>

              --Mark the bootstrap as "not yet complete"...
              initiate_transaction  <= '0';
              bootstrap_in_progress <= '0';

              --Reset the total number of cycles waited.
              cycles_waited <= (others => '0');

              state <= WAIT_FOR_INITIALIZATION;


            --
            -- Wait for the SPI flash to start up; this allows our SPI lines to be set to the proper
            -- levels prior to the CS line going low.
            --
            when WAIT_FOR_INITIALIZATION =>

              initiate_transaction <= '0';

              --Count the number of cycles waited...
              cycles_waited <= cycles_waited + 1;
             
              --... and if we've exceeded the flash's start-up time, 
              if cycles_waited >= startup_delay then 
                state <= START_BOOTSTRAP_READ;
              else
                state <= WAIT_FOR_INITIALIZATION;
              end if;


            --
            -- Begin the process of bootstrapping the device
            -- by queueing transmisison of the SPI read command.
            --
            when START_BOOTSTRAP_READ =>

              --Mark the bootstrap as "not yet complete"...
              bootstrap_in_progress <= '1';

              --Start from the beginning of our local memory.
              bootstrap_memory_address <= (others => '0');

              --Start an SPI transaction, and send the "fast read" command.
              --initiate_transaction  <= '1';
              continue_transaction  <= '1';

              --Request transmission of an SPI fast read from the device.
              spi_request_enqueue <= '1';
              spi_bytes_to_queue  <= COMMAND_READ_FROM_START'length;
              spi_data_to_queue   <= COMMAND_READ_FROM_START;

              --... and then wait for the read to complete.
              state <= BOOTSTRAP_SEND_READ_COMMAND;


            --
            -- Once an SPI read command has been enqueued,
            -- wait for the command's execution to complete.
            --
            when BOOTSTRAP_SEND_READ_COMMAND =>
  
              --Mark this as a continued transaction: we'll continue 
              continue_transaction <= '1';

              --If this is the first byte in the transmission, ensure that
              --we're attempting to initate a transmission.
              if spi_bytes_remaining = COMMAND_READ_FROM_START'length then
                initiate_transaction <= '1';

              --Or, if we've just finished a transmission, continue bootstrapping.
              elsif spi_queued_transmit_complete = '1' then
                state <= BOOTSTRAP_POPULATE_MEMORY_LOW_BYTE;
              end if;


            --
            -- Read the low byte of a pair.
            --
            -- Our flash memory is organized into 16b words; so we'll need to
            -- program once per every two bytes. In this state, we'll read the
            -- first of our two bytes, and buffer it for later writing to our
            -- internal memory.
            --
            when BOOTSTRAP_POPULATE_MEMORY_LOW_BYTE =>

              continue_transaction <= '1';

              --When we've recieved a given piece of data...
              if spi_data_complete = '1' then

                --Place the data on the input of the local memory...
                bootstrap_data_to_local_memory(7 downto 0) <= received_data;

                --... and continue to populate the memory's high byte.
                state <= BOOTSTRAP_POPULATE_MEMORY_HIGH_BYTE;

              end if;

            --
            -- Read the high byte of a pair.
            --
            -- This state always follows the read of a low byte; and allows us to read
            -- the second byte of a pair. Once the second byte is read, the two bytes
            -- are written together to our local memory.
            --
            --
            when BOOTSTRAP_POPULATE_MEMORY_HIGH_BYTE =>

              continue_transaction <= '1';

              --When we've recieved a given piece of data...
              if spi_data_complete = '1' then

                --Place the data on the input of the local memory, 
                --and queue a local memory write.
                bootstrap_data_to_local_memory(15 downto 8) <= received_data;
                bootstrap_write_to_local_memory  <= '1';


                --If we're about to exceed the size of the memory, complete the boostrap
                --and move on to the idle state.
                if bootstrap_memory_address >= (memory_size * 1024 - 1) then
                  state <= IDLE;

                --Otherwise, continue bootstrapping.
                else
                  state <= BOOTSTRAP_INCREMENT_ADDRESS;
                end if;

              end if;

            
            --
            -- After we've populated a given address in local memory,
            -- move on to he next address, and continue bootstrapping.
            --
            when BOOTSTRAP_INCREMENT_ADDRESS =>
              bootstrap_memory_address <= bootstrap_memory_address + 1;
              state <= BOOTSTRAP_POPULATE_MEMORY_LOW_BYTE;

            --
            -- Idle state; waits for further instruction.
            --
            when IDLE =>

              bootstrap_in_progress <= '0';
              null;



          end case;


        end if;
      end if;
    end process;

end spi_flash_buffered;

