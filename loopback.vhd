--------------------------------------------------------------------------------
-- UART
-- Simple loopback
--           
-- @author         Peter A Bennett
-- @copyright      (c) 2012 Peter A Bennett
-- @license        LGPL      
-- @email          pab850@googlemail.com
-- @contact        www.bytebash.com
--
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity LOOPBACK is
    port 
    (  
        -- General
        CLK                     :   in      std_ulogic;
        RESET                   :   in      std_ulogic := '0';    
        RX                      :   in      std_ulogic;
        TX, tx_mirror, rx_mirror                      :   out     std_ulogic
        
    );
end LOOPBACK;

architecture RTL of LOOPBACK is
    ----------------------------------------------------------------------------
    -- UART constants
    ----------------------------------------------------------------------------
    
    constant BAUD_RATE            : positive := 9600;
    constant CLK_FREQUENCY        : positive := 32000000;
    
    ----------------------------------------------------------------------------
    -- Component declarations
    ----------------------------------------------------------------------------
    component UART is
        generic (
                BAUD_RATE           : positive;
                CLOCK_FREQUENCY     : positive
            );
        port (  -- General
                CLOCK               :   in      std_ulogic;
                RESET               :   in      std_ulogic;    
                DATA_STREAM_IN      :   in      std_ulogic_vector(7 downto 0);
                DATA_STREAM_IN_STB  :   in      std_ulogic;
                DATA_STREAM_IN_ACK  :   out     std_ulogic;
                DATA_STREAM_OUT     :   out     std_ulogic_vector(7 downto 0);
                DATA_STREAM_OUT_STB :   out     std_ulogic;
                DATA_STREAM_OUT_ACK :   in      std_ulogic;
                TX                  :   out     std_ulogic;
                RX                  :   in      std_ulogic
             );
    end component UART;
    
    ----------------------------------------------------------------------------
    -- UART signals
    ----------------------------------------------------------------------------
    
    signal uart_data_in             : std_ulogic_vector(7 downto 0);
    signal uart_data_out            : std_ulogic_vector(7 downto 0);
    signal uart_data_in_stb         : std_ulogic;
    signal uart_data_in_ack         : std_ulogic;
    signal uart_data_out_stb        : std_ulogic;
    signal uart_data_out_ack        : std_ulogic;

    signal tx_sig : std_ulogic;
  
begin

    tx        <= tx_sig;
    tx_mirror <= tx_sig;
    rx_mirror <= rx;

    ----------------------------------------------------------------------------
    -- UART instantiation
    ----------------------------------------------------------------------------

    UART_inst1 : UART
    generic map (
            BAUD_RATE           => BAUD_RATE,
            CLOCK_FREQUENCY     => CLK_FREQUENCY
    )
    port map    (  
            -- General
            CLOCK               => CLK,
            RESET               => RESET,
            DATA_STREAM_IN      => uart_data_in,
            DATA_STREAM_IN_STB  => uart_data_in_stb,
            DATA_STREAM_IN_ACK  => uart_data_in_ack,
            DATA_STREAM_OUT     => uart_data_out,
            DATA_STREAM_OUT_STB => uart_data_out_stb,
            DATA_STREAM_OUT_ACK => uart_data_out_ack,
            TX                  => tx_sig,
            RX                  => RX
    );
    
    ----------------------------------------------------------------------------
    -- Simple loopback, retransmit any received data
    ----------------------------------------------------------------------------
    
    UART_LOOPBACK : process (CLK)
    begin
        if rising_edge(CLK) then
            if RESET = '1' then
                uart_data_in_stb        <= '0';
                uart_data_out_ack       <= '0';
                uart_data_in            <= (others => '0');
            else
                -- Acknowledge data receive strobes and set up a transmission
                -- request
                uart_data_out_ack       <= '0';
                if uart_data_out_stb = '1' then
                    uart_data_out_ack   <= '1';
                    uart_data_in_stb    <= '1';
                    uart_data_in        <= uart_data_out;
                end if;
                
                -- Clear transmission request strobe upon acknowledge.
                if uart_data_in_ack = '1' then
                    uart_data_in_stb    <= '0';
                end if;
            end if;
        end if;
    end process;
            
end RTL;
