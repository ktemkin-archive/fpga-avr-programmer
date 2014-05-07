library ieee;
use ieee.std_logic_1164.all;
 
entity test_loopback is
end test_loopback;
 
architecture behavior of test_loopback is 
 
   --The circuit's simulated inputs...
   signal clk : std_logic := '0';
   signal reset : std_logic := '0';
   signal rx : std_logic := '1';

 	--And the circuit's simulated outputs.
   signal tx : std_logic;


   constant clk_period : time := 31.25 ns;
   constant bit_period : time := 7.5 us;
   constant to_rx : std_logic_vector := "01010101011111";
 
begin
 
	-- instantiate the unit under test (uut)
   uut: entity work.loopback port map (
          clk => clk,
          reset => reset,
          rx => rx,
          tx => tx
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

end;
