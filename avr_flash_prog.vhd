--  
-- AVR-910 Compliant In-System Programmer for Soft-IP AVR-compatible microcontrollers.
--
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

--TODO: move this to the relevant library!
library avr_programmer;
use avr_programmer.constructs.all;


entity avr_flash_prog is
  generic(
    
    --The clock frequency at which the device is being run.
    --Handled by 
    clock_frequency  : positive          := 32000000;

    --The bit rate at which the programmer will communnicate.
    baud_rate        : positive          := 19200;

    --The signature of the microcontroller to report.
    signature        : std_ulogic_vector := x"FFFFFF";

    --The "programmer type", as reported upon request.
    programmer_type  : programmer_id     := (x"41", x"56", x"52", x"50", x"52", x"4F", x"47") --"AVRPROG"

  );
  port(
    
    --System-wide inputs.
    clk     : in std_ulogic;
    reset   : in std_ulogic := '0';
    
    --UART control interface.
    rx      : in  std_ulogic;
    tx      : out std_ulogic;
  

    --DEBUG only!
    rx_mirror, tx_mirror : out std_ulogic;

    --Flash memory control I/O.
    erase   : out std_ulogic;

    --TODO: Fix width.
    address : out std_ulogic_vector(15 downto 0) := (others => '0');

    --CPU-related signals.
    in_programming_mode : buffer std_ulogic := '0'
      
  );
end avr_flash_prog;


architecture behavioral of avr_flash_prog is
  
  --Signals which convey transmitted and received data to/from the UART.
  signal data_to_transmit, received_data : std_ulogic_vector(7 downto 0);

  --UART control signals.
  signal request_transmit, transmit_request_acknowledged : std_ulogic; 
  signal data_waiting, data_was_waiting_last_cycle       : std_ulogic; 
  signal new_data_received, data_reciept_handled         : std_ulogic;

  -- FSM state type for the main controller.
  type programmer_state_type is (WAIT_FOR_COMMAND, SEND_ID, RESPOND_TO_AUTO_INC_QUERY,
                                 RECIEVE_ADDR_HIGH, RECIEVE_ADDR_LOW,
                                 UNKNOWN_COMMAND, ACKNOWLEDGE_COMMAND, WAIT_FOR_TRANSMIT);


  signal state : programmer_state_type := WAIT_FOR_COMMAND;

  -- "ID transmission" sub-fsm signals.
  signal programmer_id_character    : byte;
  signal position_in_programmer_id  : integer range 0 to 6 := 0;
  signal transmitting_programmer_id, transmitting_last_byte_of_programmer_id : std_ulogic;




  --DEBUG ONLY
  signal tx_signal : std_ulogic;

begin

  --
  -- Instantiate the main UART, which handles communications
  -- with the programming PC.
  -- 
  PC_CONNECTION: entity work.uart
  generic map (
    baud_rate           => baud_rate,
    clock_frequency     => clock_frequency
  )
  port map(  
    -- general
    clock               => clk,
    reset               => reset,
    data_stream_in      => data_to_transmit,
    data_stream_in_stb  => request_transmit,
    data_stream_in_ack  => transmit_request_acknowledged,

    data_stream_out     => received_data,
    data_stream_out_stb => data_waiting,
    data_stream_out_ack => data_reciept_handled,
    tx                  => tx_signal, --tx,
    rx                  => rx
  );

  --DEBUG
  tx        <= tx_signal;
  tx_mirror <= tx_signal;
  rx_mirror <= rx;

  --Create a simple signal which goes high for exactly one cycle when new data is ready.
  data_was_waiting_last_cycle <= data_waiting when rising_edge(clk);
  new_data_received           <= data_waiting and not data_was_waiting_last_cycle; 

  --Create the main controller logic for the main FSM.
  process(clk)
  begin

    if rising_edge(clk) then

      --Set control signals to '0' unless explicitly asserted.
      data_reciept_handled       <= '0';    
      request_transmit           <= '0';
      transmitting_programmer_id <= '0';

      case state is

        --
        -- Idle state:
        -- Wait for information to be retrieved via serial.
        --
        when WAIT_FOR_COMMAND =>

          --If we've recieved new data...
          if new_data_received = '1' then

            --... indicate that the data was handled...
            data_reciept_handled <= '1';
         
            --... and move to the appropriate handler routine. 
            case received_data is
              
              --"P": enter programmming mode.
              when x"50" =>
                in_programming_mode <= '1';
                state <= ACKNOWLEDGE_COMMAND;

              --"a": A query which determines whether the device supports auto-incrementation
              --     of addresses.
              when x"61" =>
                state <= RESPOND_TO_AUTO_INC_QUERY;

              --"A": Set the current working address.
              when x"41" =>
                state <= RECIEVE_ADDR_HIGH;


              --"S", send Programmer Type.
              when x"53" => 
                state <= SEND_ID;

              --In all other cases, respond with a question mark.
              when others =>
                state <= UNKNOWN_COMMAND;
            end case;

          end if;



        --
        -- Send programmer ID.
        -- Delegate control to the ID child FSM.
        --
        when SEND_ID =>

          --Send the active bit of the programmer id.
          --When 
          data_to_transmit <= programmer_type(position_in_programmer_id);
          request_transmit <= '1';

          --Notify the child FSM.
          transmitting_programmer_id <= '1';

          --Once we're finished enquing the last byte of the programmer ID,
          --move to the WAIT_FOR_TRANSMIT state, and start over.
          if transmitting_last_byte_of_programmer_id = '1' then
            state <= WAIT_FOR_TRANSMIT;
          end if;


        --
        -- Acknowledge valid command.
        -- A valid command has been completed; send back an acknowledgement.
        --
        when RESPOND_TO_AUTO_INC_QUERY =>

          --Respond with a capital 'Y', indicating that we do support auto-incrementation.
          data_to_transmit <= x"59";
          request_transmit <= '1';
          state <= WAIT_FOR_TRANSMIT;


        --
        -- Recieve Address High.
        -- Recieve and set the target address's high bit.
        --
        when RECIEVE_ADDR_HIGH =>

          -- Once we've received a byte of data,
          -- use it to set the high byte of the address, then
          -- move to the low byte.
          if new_data_received = '1' then
            address(15 downto 8) <= received_data;
            data_reciept_handled <= '1';
            state <= RECIEVE_ADDR_LOW;
          end if;
             

        --
        -- Recieve Address High.
        -- Recieve and set the target address's high bit.
        --
        when RECIEVE_ADDR_LOW =>

          -- Once we've received a byte of data,
          -- use it to set the high byte of the address, then
          -- move to the low byte.
          if new_data_received = '1' then
            address(7 downto 0) <= received_data;
            data_reciept_handled <= '1';
            state <= ACKNOWLEDGE_COMMAND;
          end if;



        --
        -- Recieved Unknown Command.
        -- We've recieved an unknown command, and should respond with a '?'.
        --
        when UNKNOWN_COMMAND =>

          --Set up the UART to send back a '?'
          data_to_transmit <= x"3F";
          request_transmit <= '1';
          state <= WAIT_FOR_TRANSMIT;


        --
        -- Acknowledge valid command.
        -- A valid command has been completed; send back an acknowledgement.
        --
        when ACKNOWLEDGE_COMMAND =>

          --Set up the UART to send back a carriage return "\r".
          data_to_transmit <= x"3F";
          request_transmit <= '1';
          state <= WAIT_FOR_TRANSMIT;

        
        --
        -- Wait for transmission.
        -- A transmission has been started; wait until it's complete, and
        -- and then start from the beginning.
        --
        when WAIT_FOR_TRANSMIT =>

          --Continue to request transmission until the UART
          --acknowledges that our transmission is complete.
          request_transmit <= '1';

          --Once we've recieved an acknowledgement,
          --move to wait for our next command.
          if transmit_request_acknowledged = '1' then
            state <= WAIT_FOR_COMMAND;
          end if;

      end case;
    end if;
  end process;


  --
  -- ID transmission sub-FSM.
  -- 
  process(clk)
  begin
    if rising_edge(clk) then

      --If we're not supposed to be transmitting, 
      if reset = '1' or transmitting_programmer_id = '0' then
        position_in_programmer_id <= 0;

      --If we've just finished transmitting a bit of the programmer id...
      elsif transmit_request_acknowledged = '1' and transmitting_programmer_id = '1' then
        position_in_programmer_id <= position_in_programmer_id + 1;
      end if;

    end if;
  end process;

  --Determine when we're transmitting the last byte of the programmer ID.
  transmitting_last_byte_of_programmer_id <= '1' when (position_in_programmer_id = programmer_id'high) else '0';





end behavioral;

