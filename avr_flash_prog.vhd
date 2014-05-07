--  
-- AVR-910 Compliant In-System Programmer for 
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
    programmer_type  : string            := "AVRPROG"
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
    address : out std_ulogic_vector(7 downto 0);

    --CPU-related signals.
    in_programming_mode : buffer std_ulogic
      
  );
end avr_flash_prog;


architecture behavioral of avr_flash_prog is
  
  --Signals which convey transmitted and received data to/from the UART.
  signal data_to_transmit, received_data : std_ulogic_vector(7 downto 0);

  --UART control signals.
  signal request_transmit, transmit_request_acknowledged : std_ulogic; 
  signal new_data_received, data_reciept_handled : std_ulogic;

  -- FSM state type for the main controller.
  type programmer_state_type is (WAIT_FOR_COMMAND, SEND_ID, UNKNOWN_COMMAND, WAIT_FOR_TRANSMIT);
  signal state : programmer_state_type;

  --DEBUG ONLY
  signal tx_signal : std_ulogic;

begin

  --
  -- Instantiate the main UART, which handles communications
  -- with the programming PC.
  -- 
  PC_CONNECTION: entity uart
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
    data_stream_out_stb => new_data_received,
    data_stream_out_ack => data_reciept_handled,
    tx                  => tx_signal, --tx,
    rx                  => rx
  );

  --DEBUG
  tx        <= tx_signal;
  tx_mirror <= tx_signal;
  rx_mirror <= rx;

  --Create the main controller logic for the main FSM.
  process(clk)
  begin

    if rising_edge(clk) then

      --Set control signals to '0' unless explicitly asserted.
      data_reciept_handled <= '0';    

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
              
              --"P", enter programmming mode.
              --when x"50" => 
              --next_state <= SEND_ID;

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

        --
        -- Recieved Unknown Command.
        -- We've recieved an unknown command, and should respond with a '?'.
        --
        when UNKNOWN_COMMAND =>

          --Set up the UART to send back a '?'
          data_to_transmit <= x"3F";
          request_transmit <= '1';

          --... and wait until the transmission is complete.
          state <= WAIT_FOR_TRANSMIT;


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



  end process;





end behavioral;

