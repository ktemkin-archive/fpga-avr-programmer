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
    --address : in std_logic_vector(integer(ceil(log2(real(memory_size *10)))) - 1 downto 0);
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
  signal data_to_transmit, received_data : std_ulogic_vector(7 downto 0);

  --SPI control and status flags.
  signal spi_is_busy, spi_was_busy, spi_data_accepted, spi_data_complete : std_ulogic;
  signal initiate_transaction, continue_transaction : std_ulogic := '0';

  --SPI command constants.
  signal COMMAND_FAST_READ : std_ulogic_vector(7 downto 0) := x"0B";

  --Local memory data exchange signals.
  signal local_memory_address     : std_ulogic_vector(15 downto 0) := (others => '0');
  signal bootstrap_memory_address : unsigned(15 downto 0)          := (others => '0');

  signal data_to_local_memory, data_from_local_memory : std_ulogic_vector(15 downto 0);
  signal write_to_local_memory, clear_local_memory, bootstrap_in_progress : std_ulogic;

  --"Wait time" counter.
  signal cycles_waited : unsigned(11 downto 0);

  --Signals for the core FSM.
  type state_type is 
    (
      STARTUP,
      WAIT_FOR_INITIALIZATION,
      START_BOOTSTRAP_READ,
      BOOTSTRAP_SEND_ADDRESS_MSB,
      BOOTSTRAP_SEND_ADDRESS_MIDDLE,
      BOOTSTRAP_SEND_ADDRESS_LSB,
      BOOTSTRAP_QUEUE_DUMMY_CYCLE,
      BOOTSTRAP_WAIT_DUMMY_CYCLE,
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
      erase    => clear_local_memory,

      address  => local_memory_address,
      data_in  => data_to_local_memory,
      data_out => data_from_local_memory
    );

  --
  -- Address control for the local memory.
  --
  local_memory_address <=
    std_ulogic_vector(bootstrap_memory_address) when bootstrap_in_progress = '1' else
    address;

  
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
      tx_data => data_to_transmit,
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
    -- Main controller FSM for the whole device.
    --
    CONTROLLER:
    process(clk, cycles_waited)
    begin
      if rising_edge(clk) then

        if reset = '1' then
          state <= STARTUP;
        else

          --Keep the following signals low unless explicitly assertd.
          initiate_transaction  <= '0';
          continue_transaction  <= '0';
          write_to_local_memory <= '0';
          clear_local_memory    <= '0';

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
            -- Initial "bootstrap" state.
            --
            -- In this state, the memory will attempt to bootstrap its fast local SRAM
            -- from the SPI flash, with the intent of providing a quickly-accessible mirror 
            -- of the AVR program in flash.
            --
            when START_BOOTSTRAP_READ =>

              --Mark the bootstrap as "not yet complete"...
              bootstrap_in_progress <= '1';

              --Start from the beginning of our local memory.
              bootstrap_memory_address <= (others => '0');

              --Start an SPI transaction, and send the "fast read" command.
              data_to_transmit      <= COMMAND_FAST_READ;
              initiate_transaction  <= '1';
              continue_transaction  <= '1';

              --Once the SPI controller has accepted the byte, move to the SEND_BOOTSTRAP_ADDRESS
              --state.
              if spi_data_accepted = '1' then
                state <= BOOTSTRAP_SEND_ADDRESS_MSB;
              end if;


            --
            -- Send the most significant bit of the address at which the AVR program is located.
            --
            when BOOTSTRAP_SEND_ADDRESS_MSB =>

              --Continue to transmit the MSB of the start address...
              continue_transaction <= '1';
              data_to_transmit     <= start_address(23 downto 16);

              
              if spi_data_accepted = '1' then
                state <= BOOTSTRAP_SEND_ADDRESS_MIDDLE;
              end if;


            --
            -- Send the most significant bit of the address at which the AVR program is located.
            --
            when BOOTSTRAP_SEND_ADDRESS_MIDDLE =>

              data_to_transmit <= start_address(15 downto 8);
              continue_transaction <= '1';

              if spi_data_accepted = '1' then
                state <= BOOTSTRAP_SEND_ADDRESS_LSB;
              end if;


            --
            -- Send the most significant bit of the address at which the AVR program is located.
            --
            when BOOTSTRAP_SEND_ADDRESS_LSB =>

              data_to_transmit     <= start_address(7 downto 0);
              continue_transaction <= '1';

              --Once the LSB address has been accepted,
              --prepare for the read.
              if spi_data_accepted = '1' then
                state <= BOOTSTRAP_QUEUE_DUMMY_CYCLE;
              end if;


            --
            -- Waits a single SPI write cycle, giving the SPI flash time to prepare for a fast write.
            --
            when BOOTSTRAP_QUEUE_DUMMY_CYCLE =>

              continue_transaction <= '1';

              --Once the dummy instruction has been accepted,
              --move on to our second wait state.
              --if spi_data_accepted = '1' then
              if spi_data_accepted = '1' then
                state <= BOOTSTRAP_WAIT_DUMMY_CYCLE;
              end if;


            --
            -- Waits until the dummy cycle is complete.
            --
            when BOOTSTRAP_WAIT_DUMMY_CYCLE =>

              continue_transaction <= '1';

              --Wait for the SPI dummy instruction to be complete.
              if spi_data_complete = '1' then
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
                data_to_local_memory(7 downto 0) <= received_data;

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
                data_to_local_memory(15 downto 8) <= received_data;
                write_to_local_memory             <= '1';


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

