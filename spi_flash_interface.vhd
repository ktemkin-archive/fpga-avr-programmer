--  
-- AVR-910 Compliant In-System Programmer for Soft-IP AVR-compatible microcontrollers.
-- SPI flash layer for working with external flash memories.
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

entity spi_program_memory is 
  generic(
    
    --The address in the SPI flash at which AVR programs are stored.
    --For the built-in erase functionality to work, this should be a
    --multiple of 4KiB (0x1000).
    start_address : in std_ulogic_vector(23 downto 0) := x"050000"; 

    --The number of clock cycles to delay before starting an SPI
    --transaction, after a reset.
    startup_delay : in integer := 4095;

    --The size of the given memory, in kilobytes.
    --
    --To be compatible with industry-standard flash memory
    --erase scheme this should be a multiple of four.
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

    --Data in and out.
    data_in  : in  word;
    data_out : out word;

    --Status flags.
    bootstrap_complete : out std_ulogic := '0';

    --DEBUG
    transact_enable, busy  : out std_ulogic


  );
end spi_program_memory;



architecture spi_flash_buffered of spi_program_memory is

  constant KILOBYTES_PER_PAGE : integer := 4;

  --
  --SPI command constants.
  --
  constant COMMAND_FAST_READ                         : byte := x"0B";
  constant COMMAND_ENABLE_WRITE_TO_STATUS_REGISTER   : byte := x"50";
  constant COMMAND_WRITE_TO_STATUS_REGISTER          : byte := x"01";
  constant COMMAND_WRITE_ENABLE                      : byte := x"06";
  constant COMMAND_ERASE_FOUR_KILOBYTES              : byte := x"20";

  --The full command to schedule a fast read from the relevant starting address in the SPI flash.
  constant COMMAND_READ_FROM_START          : byte_vector := (COMMAND_FAST_READ, start_address(23 downto 16), start_address(15 downto 8), start_address(7 downto 0), x"00");
  constant COMMAND_DISABLE_WRITE_PROTECTION : byte_vector := (COMMAND_WRITE_TO_STATUS_REGISTER, x"00");
  constant COMMAND_PREFIX_ERASE_PAGE        : byte_vector := (0 => COMMAND_ERASE_FOUR_KILOBYTES);

  --Timing constants for SPI writes.
  constant CYCLES_FOR_FOUR_KILOBYTE_ERASE   : integer     := 80000;

  --Local memory constants.
  constant HIGHEST_LOCAL_MEMORY_ADDRESS : integer := (memory_size * 1024 - 1);


  --Local memory data exchange signals.
  signal local_memory_address : std_ulogic_vector(15 downto 0) := (others => '0');
  signal data_to_local_memory, data_from_local_memory : std_ulogic_vector(15 downto 0);
  signal write_to_local_memory, clear_local_memory, bootstrap_in_progress : std_ulogic;

  --Bootstrap copies of the local memory data exchange signals.
  signal bootstrap_memory_address : unsigned(15 downto 0) := (others => '0');
  signal bootstrap_data_to_local_memory, bootstrap_data_from_local_memory : std_ulogic_vector(15 downto 0);
  signal bootstrap_write_to_local_memory : std_ulogic;

  --Signals for the "chip erase" mode.
  constant memory_size_in_pages : integer := memory_size / KILOBYTES_PER_PAGE;
  signal   pages_to_erase       : integer range 0 to memory_size_in_pages;

  --"Wait time" counter.
  signal cycles_waited : unsigned(19 downto 0);

  --Signals for the core FSM.
  type state_type is 
    (
      --Device startup states.
      STARTUP,
      WAIT_FOR_INITIALIZATION,

      --Initial bootstrap states.
      START_BOOTSTRAP_READ,
      BOOTSTRAP_SEND_READ_COMMAND,
      BOOTSTRAP_POPULATE_MEMORY_HIGH_BYTE,
      BOOTSTRAP_POPULATE_MEMORY_LOW_BYTE,
      BOOTSTRAP_INCREMENT_ADDRESS,

      --Erase memory states.
      CHIP_ERASE_DISABLE_WRITE_PROTECT,
      CHIP_ERASE_WRITE_ENABLE,
      CHIP_ERASE_ERASE_PAGE,
      CHIP_ERASE_WAIT_FOR_ERASE, 

      IDLE
    );
  signal state : state_type := STARTUP;


  signal spi_ready_for_continuation, spi_data_complete, spi_transmit_complete : std_ulogic;
  signal spi_enqueuing_starts_transmission, spi_request_enqueue, spi_receive : std_ulogic;
  signal spi_last_received_byte : byte;
  signal spi_data_to_transmit : byte_vector(0 to 4);
  signal spi_transmission_length : integer range 0 to spi_data_to_transmit'high;
  signal spi_ready_for_new_transmission : std_ulogic;


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
  -- Core transmitter.
  --
  QUEUEING_TRANSMITTER:
  entity work.spi_queueing_master
    port map(

      clk                => clk,
      reset              => reset,

      --Transmitter data inputs.
      data_to_queue      => spi_data_to_transmit,
      data_length        => spi_transmission_length,
      request_enqueue    => spi_request_enqueue,

      --Transmitter data outputs.
      receive            => spi_receive,
      last_received_byte => spi_last_received_byte,
      new_byte_received  => spi_data_complete,

      --start_transmission => spi_enqueuing_starts_transmission,
      
      --SPI bus signals.
      flash_sck          => flash_sck,
      flash_miso         => flash_miso,
      flash_mosi         => flash_mosi,
      flash_cs           => flash_cs,

      --SPI progress signals.
      ready_for_continuous_enqueue  => spi_ready_for_continuation,
      ready_for_new_packet_enqueue  => spi_ready_for_new_transmission,
      transmit_complete             => spi_transmit_complete
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
    -- Main controller flags.
    --
    bootstrap_complete <= not bootstrap_in_progress;

    --DEBUG
    transact_enable <= spi_transmit_complete;
    busy <= spi_ready_for_new_transmission;


    --
    -- Main controller FSM for the whole device.
    --
    CONTROLLER:
    process(clk)
    
      --
      --Shorthand procedure which enqueues a set of bytes to be transmitted.
      -- command: The command to enqueue, as a collection of ordered bytes.
      -- starts_transaction: True iff the given transmission should start a transaction.
      procedure spi_transmit(command : in byte_vector) is
      begin
        spi_request_enqueue                  <= '1';
        spi_transmission_length              <= command'length;
        spi_data_to_transmit(command'range)  <= command;
      end procedure;

      --
      -- Function which determines the SPI flash address for the given page number,
      -- where each page is a 4KiB flash page.
      --
      impure function address_for_page_number(page_number : integer) return byte_vector is
        variable target_address : unsigned(23 downto 0);
        variable address_vector : std_ulogic_vector(23 downto 0);
      begin
        --Compute the target address. 
        target_address := unsigned(start_address) + (KILOBYTES_PER_PAGE * 1024 * page_number);
        
        --...and convert it to std_ulogic_vector...
        address_vector := std_ulogic_vector(target_address);

        --... and then into a vector of bytes.
        return (address_vector(23 downto 16), address_vector(15 downto 8), address_vector(7 downto 0));

      end function;


    begin
      if rising_edge(clk) then

        --Synchronous reset.
        if reset = '1' then
          state <= STARTUP;
        else

          --Keep the following signals low unless explicitly asserted.
          spi_receive                       <= '0';
          bootstrap_write_to_local_memory   <= '0';
          clear_local_memory                <= '0';
          spi_request_enqueue               <= '0';

          --Provide default values for the queuing transmitter data lines;
          --this prevents us from needlessly inferring a memory.
          spi_data_to_transmit              <= (others => x"00");
          spi_transmission_length           <= 0;


          case state is

            --
            -- Startup (initialization) state.
            --
            when STARTUP =>

              --Mark the bootstrap as "not yet complete"...
              bootstrap_in_progress <= '0';

              --Reset the total number of cycles waited.
              cycles_waited <= (others => '0');

              --... and move to the initialization state.
              state <= WAIT_FOR_INITIALIZATION;


            --
            -- Wait for the SPI flash to start up; this allows our SPI lines to be set to the proper
            -- levels prior to the CS line going low.
            --
            when WAIT_FOR_INITIALIZATION =>

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

              --Request transmission of an SPI fast read from the device.
              spi_transmit(COMMAND_READ_FROM_START);
              
              --... and then wait for the read to complete.
              state <= BOOTSTRAP_SEND_READ_COMMAND;


            --
            -- Once an SPI read command has been enqueued,
            -- wait for the command's execution to complete.
            --
            when BOOTSTRAP_SEND_READ_COMMAND =>
  
              --Mark this as a continued transaction: we'll continue 
              --continue_transaction <= '1';
              spi_receive <= '1';

              --Once we've finished a transmission, continue bootstrapping.
              if spi_transmit_complete = '1' then
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

              spi_receive <= '1';

              --When we've recieved a given piece of data...
              if spi_data_complete = '1' then

                --Place the data on the input of the local memory...
                bootstrap_data_to_local_memory(7 downto 0) <= spi_last_received_byte;

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

              spi_receive <= '1';

              --When we've recieved a given piece of data...
              if spi_data_complete = '1' then

                --Place the data on the input of the local memory, 
                --and queue a local memory write.
                bootstrap_data_to_local_memory(15 downto 8) <= spi_last_received_byte;
                bootstrap_write_to_local_memory  <= '1';

                --If we're about to exceed the size of the memory, complete the boostrap
                --and move on to the idle state.
                if bootstrap_memory_address >= HIGHEST_LOCAL_MEMORY_ADDRESS then
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

              --If a chip erase has been requested:
              if erase = '1' then

                --Disable write protection...
                spi_transmit((0 => COMMAND_ENABLE_WRITE_TO_STATUS_REGISTER));

                --... record the number of pages to be erased...
                pages_to_erase <= memory_size_in_pages;

                --... and move on to erasing the chip.
                state <= CHIP_ERASE_DISABLE_WRITE_PROTECT;

              end if;


            --
            -- State in which write 
            --
            when CHIP_ERASE_DISABLE_WRITE_PROTECT =>

              --Once we're ready for the next transmission, continue with the appropriate command. 
              if spi_ready_for_new_transmission = '1' then

                --Disable write protection...
                spi_transmit(COMMAND_DISABLE_WRITE_PROTECTION);

                --... and move on to erasing the chip.
                state <= CHIP_ERASE_WRITE_ENABLE;


              end if;

            --
            -- Waits for the SPI transmit line to be free,
            -- and then queues a write enable.
            --
            when CHIP_ERASE_WRITE_ENABLE =>

              --Wait for the SPI bus to be free.
              if spi_ready_for_new_transmission = '1' then
                
                --... queue a transmit of the "write enable" command...
                spi_transmit((0 => COMMAND_WRITE_ENABLE));

                --... and continue to the erase page.
                cycles_waited <= (others => '0');
                state <= CHIP_ERASE_ERASE_PAGE;

              end if;



            --
            -- Perform an ordered "chip erase" by erasing the relevant
            -- SPI flash region, which starts at the provided "start_address"
            -- and extends for n-kilobytes.
            --
            when CHIP_ERASE_ERASE_PAGE =>


              if spi_ready_for_new_transmission = '1' then

                --Transmit the command to erase the given page, and...
                spi_transmit(COMMAND_ERASE_FOUR_KILOBYTES & address_for_page_number(pages_to_erase - 1));

                --... and move to the next "write enable" state.
                state <= CHIP_ERASE_WAIT_FOR_ERASE;

                --... and decrease the total pages to erase.
                pages_to_erase <= pages_to_erase - 1;

              end if;


            --
            -- Wait for the flash to complete the chip erase.
            --
            -- In this case, we wait the maximum erasure time
            -- as specified in the datasheet. This could
            -- potentially be improved by polling for the write
            -- status, if necessary for the given applicaiton.
            --
            when CHIP_ERASE_WAIT_FOR_ERASE =>

              --Count the number of waited cycles...
              cycles_waited <= cycles_waited + 1;

              --If we've waitedd for longer than the sector erase time,
              --move to the next state, as appropriate.
              if cycles_waited > CYCLES_FOR_FOUR_KILOBYTE_ERASE then

                --If we have pages left to erase, continue the erasure.
                if pages_to_erase > 0 then 
                  state <= CHIP_ERASE_WRITE_ENABLE;

                --Otherwise, move back to the idle state.
                else
                  state <= IDLE;
                end if;

              end if;

          end case;

        end if;
      end if;
    end process;

end spi_flash_buffered;

