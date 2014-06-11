library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
 
entity spi_program_memory_simple_test is
end spi_program_memory_simple_test;
 
architecture behavior of spi_program_memory_simple_test is 

   -- component declaration for the unit under test (uut)
   component spi_program_memory
   port(
        clk : in  std_ulogic;
        enable : in  std_ulogic;
        reset : in  std_ulogic;
        flash_sck : buffer  std_ulogic;
        flash_miso : in  std_ulogic;
        flash_mosi : buffer  std_ulogic;
        flash_cs : buffer  std_ulogic;
        write : in  std_ulogic;
        erase : in  std_ulogic;
        address : in  std_ulogic_vector(15 downto 0);
        data_in : in  std_ulogic_vector(15 downto 0);
        data_out : out  std_ulogic_vector(15 downto 0);
        bootstrap_complete : out  std_ulogic;
        transact_enable : out  std_ulogic;
        busy : out  std_ulogic
       );
   end component;
   

  --inputs
  signal clk : std_ulogic := '0';
  signal enable : std_ulogic := '0';
  signal reset : std_ulogic := '0';
  signal flash_miso : std_ulogic := '0';
  signal write : std_ulogic := '0';
  signal erase : std_ulogic := '0';
  signal address : std_ulogic_vector(15 downto 0) := (others => '0');
  signal data_in : std_ulogic_vector(15 downto 0) := (others => '0');

	--outputs
  signal flash_sck : std_ulogic;
  signal flash_mosi : std_ulogic;
  signal flash_cs : std_ulogic;
  signal data_out : std_ulogic_vector(15 downto 0);
  signal bootstrap_complete : std_ulogic;
  signal transact_enable : std_ulogic;
  signal busy : std_ulogic;

  -- clock period definitions
  constant clk_period : time := 31.25 ns;
  --constant clk_period : time := 31.25 ps;

  --SPI test signals.
  signal byte_complete : boolean := false;
  signal bit_count     : unsigned(2 downto 0) := (others => '0');
  signal byte_transmitted, last_byte_transmitted : std_ulogic_vector(7 downto 0);
  signal byte_received, byte_to_recieve, next_byte_received : std_ulogic_vector(7 downto 0);

begin

  --
  -- Instantiate our Unit Under Test (UUT).
  -- 
  UUT: 
  spi_program_memory port map (
    clk => clk,
    enable => enable,
    reset => '0',
    flash_sck => flash_sck,
    flash_miso => flash_miso,
    flash_mosi => flash_mosi,
    flash_cs => flash_cs,
    write => write,
    erase => erase,
    address => address,
    data_in => data_in,
    data_out => data_out,
    bootstrap_complete => bootstrap_complete,
    transact_enable => transact_enable,
    busy => busy
   );

  --
  -- Create our core system clock.
  --
  clk <= not clk after (clk_period / 2);

  --
  -- For convenience, count the total number of SCK pulses.
  -- This helps to keep us byte aligned.
  --
  bit_count     <= bit_count + 1 when rising_edge(flash_sck);
  byte_complete <= bit_count = 0;

  --Simple SPI transmitter and reciever.
  --(Note that we don't need to worry about metastability here, as this is only used in simulation!)
  byte_transmitted      <= byte_transmitted(6 downto 0) & flash_mosi when rising_edge(flash_sck);
  last_byte_transmitted <= byte_transmitted when byte_complete and rising_edge(flash_sck);

  byte_received      <= next_byte_received when falling_edge(flash_sck);
  next_byte_received <= byte_to_recieve    when byte_complete else byte_received(6 downto 0) & '0';
  flash_miso         <= byte_received(7);


  process
  begin

    --Wait until the transmission starts, and assert that we're receiving the correct command.
    wait until flash_cs = '0' and not byte_complete;

    --Assert that the start address is sent correctly.
    wait until byte_complete;
    assert byte_transmitted = x"0B";
    wait until byte_complete;
    assert byte_transmitted = x"05";
    wait until byte_complete;
    assert byte_transmitted = x"00";
    wait until byte_complete;
    assert byte_transmitted = x"00";


    --... and start applying memory contents.
    --Note that this is 8Ki-word, where words are 16 bits,
    --the size of the AVR memory (and Xilinx block RAMs).
    for j in 0 to 63 loop
      for i in 0 to 255 loop
        byte_to_recieve <= std_ulogic_vector(to_unsigned(i, 8));
        wait until byte_complete and rising_edge(flash_sck);
      end loop;
    end loop;

    --Wait for a moment, to create a visible gap on the waveform output.
    wait for 10 ms;

    --As a test, read back each of the values...
    for i in 0 to 8191
    loop
      address <= std_ulogic_vector(to_unsigned(i, 16));
      wait until rising_edge(clk);
    end loop;

    --Halt the simulation.
    wait;
   

  end process;



end;
