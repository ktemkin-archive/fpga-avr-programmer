--  
-- AVR-910 Compliant In-System Programmer for Soft-IP AVR-compatible microcontrollers.
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

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--USE ieee.numeric_std.ALL;
 
ENTITY test_avr_flash_prog IS
END test_avr_flash_prog;
 
ARCHITECTURE behavior OF test_avr_flash_prog IS 
 
    -- Component Declaration for the Unit Under Test (UUT)
 
    COMPONENT avr_flash_prog
    PORT(
         clk : IN  std_ulogic;
         reset : IN  std_ulogic;
         rx : IN  std_ulogic;
         tx : OUT  std_ulogic;
         rx_mirror : OUT  std_ulogic;
         tx_mirror : OUT  std_ulogic;
         erase : OUT  std_ulogic;
         address : OUT  std_ulogic_vector(15 downto 0);
         in_programming_mode : buffer  std_ulogic
        );
    END COMPONENT;
    

   --Inputs
   signal clk : std_ulogic := '0';
   signal reset : std_ulogic := '0';
   signal rx : std_ulogic := '0';

 	--Outputs
   signal tx : std_ulogic;
   signal erase : std_ulogic;
   signal address : std_ulogic_vector(15 downto 0);
   signal in_programming_mode : std_ulogic;

   -- Clock period definitions
   constant clk_period : time := 31.25 ns;
   constant bit_period : time := 52.0833 us;
   --                                           P                  A                 dh                 dl
   constant to_rx : std_ulogic_vector := b"111_0_0000_1010_1_1111_0_1000_0010_1_1111_0_1001_1001_1_1111_0_0110_0110_1";
 
 
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
   uut: avr_flash_prog PORT MAP (
          clk => clk,
          reset => reset,
          rx => rx,
          tx => tx,
          erase => erase,
          address => address,
          in_programming_mode => in_programming_mode
        );

   clk <= not clk after clk_period / 2;
 

   -- stimulus process
   stim_proc: process
   begin		

      reset <= '1';

      -- hold reset state for 100 ns.
      wait for 100 ns;	
      wait for clk_period*10;

      --desassert reset
      reset <= '0';
      wait for clk_period*2;

      --and then apply test stimulus
      for i in to_rx'range loop
       rx <= to_rx(i);
       wait for bit_period;
      end loop;

    


      wait;
   end process;

END;
