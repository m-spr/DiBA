------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY tanh IS 
PORT ( 
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      tanhout : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
	   );
END tanh;	  

ARCHITECTURE behavioral OF tanh IS 

	COMPONENT mux_2 is
	  generic ( n : integer := 31);
	  port (
			sel : in std_logic;
			a , b : in STD_LOGIC_VECTOR (n DOWNTO 0);
			c :out STD_LOGIC_VECTOR(n DOWNTO 0) 
			 );
	END COMPONENT;
	
	component comp_tanh IS 
		PORT ( 
			  signed_din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
			  d : OUT std_logic;
			  p1_index , p2_index: OUT STD_LOGIC_VECTOR(4 downto 0);
			  a : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
			   );
	END component;
	
	component SIGNED_ADD IS
		generic ( n : integer := 31);
		PORT (din1, din2 : in STD_LOGIC_VECTOR(n downto 0);
			  dout : out STD_LOGIC_VECTOR(n downto 0));
	END component;

	SIGNAL a , add_out, din1, din2 , tanhout1, tanhout2:STD_LOGIC_VECTOR(18 DOWNTO 0);
    SIGNAL p1_index , p2_index: STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL d : std_logic; --direct
	signal h1 , h2 : integer;
BEGIN 
        
	comparator : comp_tanh 
		PORT map ( 
			  din ,  d,  p1_index , p2_index, a
			   );
	h1 <= to_INTEGER(unsigned (p1_index(3 DOWNTO 0)));
	h2 <= to_INTEGER(unsigned (p2_index(3 DOWNTO 0)));

    din1(17 DOWNTO 0) <=STD_LOGIC_VECTOR( shift_RIGHT(unsigned(din(17 DOWNTO 0)),h1));
	din2(17 DOWNTO 0) <=STD_LOGIC_VECTOR( shift_RIGHT(unsigned(din(17 DOWNTO 0)),h2));
  
    din1(18) <= p1_index(4) xor din(31);
    din2(18) <= p2_index(4) xor din(31);
  
    add1 : SIGNED_ADD 
		generic map (18)
		PORT map (din1, din2 , add_out);
		
	add2 : SIGNED_ADD 
		generic map (18)
		PORT map (add_out, a , tanhout1);
		
	mux1: 	mux_2
	  generic map (18)  
	  PORT map(
			din(31),
			"0100000000000000000" ,"1100000000000000000",
			tanhout2
			 );
	mux2:  mux_2
	  generic map (18)  
	  PORT map(
			d,
			tanhout1,tanhout2,
			tanhout
			 );
		
END behavioral;	 
---------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;


ENTITY comp_tanh IS 
PORT ( 
      signed_din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	  d : OUT std_logic;
	  p1_index , p2_index: OUT STD_LOGIC_VECTOR(4 downto 0);
      a : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
	   );
END comp_tanh;


ARCHITECTURE behavioural OF comp_tanh IS
signal a_index : std_logic_vector(30 downto 0);
begin 

process(signed_din)
BEGIN 
d <= '0';
p1_index <= "00000";
p2_index <= "00000";

    if (  signed_din(30 DOWNTO 18)  > "0000000000000" )then                -- x > 4
			  d <= '1';
    elsif ( "101110011001100110" =  signed_din(17 DOWNTO 0)  )then  -- x > 2.9
			  p1_index <= "01000" ;
			  p2_index <= "01010" ;
			  a_index <= "0111110110110010001100000000000";
    elsif ( "101" =  signed_din(17 DOWNTO 15)   )then                -- x > 2.5
			  p1_index <= "00110" ;
			  p2_index <= "01000" ;
			  a_index <= "0111100000010000011000000000000";
    elsif ( '1'  = signed_din(17)   )then                             -- x > 2
			  p1_index <= "00100" ;
			  p2_index <= "10110" ;
			  a_index <= "0110111101111100111100000000000";  
    elsif ( "11011100001010001" =  signed_din(16 downto 0)   )then  -- x > 1.72
			  p1_index <= "00100" ;
			  p2_index <= "00101" ;
			  a_index <= "0110001110000101001000000000000";  
    elsif ( "11000010100011110" <  signed_din(16 DOWNTO 0)  )then  -- x > 1.52
			  p1_index <= "00010" ;
			  p2_index <= "00110" ;
			  a_index <= "0101100100010110100010000000000";
    elsif ( "10100011110101110"  <  signed_din(16 DOWNTO 0) )then   -- x > 1.28
			  p1_index <= "00010" ;
			  p2_index <= "10101" ;
			  a_index <= "0100100111101011100010000000000";
    elsif ( "10001100110011001"  <  signed_din(16 DOWNTO 0)   )then  -- x> 1.1
			  p1_index <= "00010" ;
			  p2_index <= "00100" ;
			  a_index <="0011101010001111011000000000000" ;
    elsif ( "1101110000101000" <  signed_din(15 DOWNTO 0)    )then -- x> 0.86
			  p1_index <= "00001" ;
			  p2_index <= "10100" ;
			  a_index <= "0010100100110000110000000000000";
    elsif ( "1011110101110000" <  signed_din(15 DOWNTO 0)    )then -- x > 0.74
			  p1_index <= "00001" ;
			  p2_index <= "00100" ;
			  a_index <= "0001101101001011001000000000000";
    elsif ( "1010000101000111" <  signed_din(15 DOWNTO 0)    )then -- x > 0.63
			  p1_index <= "00001" ;
			  p2_index <= "00010" ;
			  a_index <= "0001010100111111100000000000000";
    elsif ( "111101011100001" <  signed_din(14 DOWNTO 0)    )then -- x > 0.48
			  p1_index <= "00001" ;
			  p2_index <= "00010" ;
			  a_index <= "0000101100110011001100000000000";
	elsif ( "111000010100011" <  signed_din(14 DOWNTO 0)    )then -- x > 0.44
			  p1_index <= "00000" ;
			  p2_index <= "10010" ;
	          a_index <= "0000001110001000011010000000000";
    elsif ( "101000111101011" <  signed_din(14 DOWNTO 0)    )then -- x > 0.32
			  p1_index <= "00000" ;
			  p2_index <= "10010" ;
			  a_index <= "0000001111010000100000000000000";
    elsif ( '1' =  signed_din(14)    )then                        -- x > 0.25
			  p1_index <= "00000" ;
			  p2_index <= "10100" ;
			  a_index <= "0000000101000100011010000000000";
    elsif ( "10011001100110" <  signed_din(13 DOWNTO 0)    )then -- x > 0.15
			  p1_index <= "00000" ;
			  p2_index <= "10101" ;
			  a_index <= "0000000001100101100110000000000";		
    else
			  p1_index <= "00001" ;
			  p2_index <= "00001" ;
			  a_index <=  "0000000000000011010010000000000" ;
      
    END if;
  END process;
  
    a <= signed_din(31)& a_index(30 downto 13);

  
END behavioural;	 
------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY reg IS 
PORT (clk , en : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	    dout : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
END reg;	  

ARCHITECTURE behavioral OF reg IS 
BEGIN 
process(clk)
 BEGIN 
  
    if  rising_edge(clk) then
      if(en = '1') then
       dout <= din;
     END if;
    END if;
    
 END process;
END behavioral;	 
------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY sigmoid IS 
PORT ( 
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      sigout : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
	   );
END sigmoid;	  

ARCHITECTURE behavioral OF sigmoid IS 


	COMPONENT  mux_2 is
	  generic ( n : integer := 31);
	  port (
			sel : in std_logic;
			a , b : in STD_LOGIC_VECTOR (n DOWNTO 0);
			c :out STD_LOGIC_VECTOR(n DOWNTO 0) 
			 );
	end COMPONENT;
	
	component comp_sig IS 
	PORT ( 
		  signed_din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
		  d : OUT std_logic;
		  p1_index , p2_index: OUT STD_LOGIC_VECTOR(4 downto 0);
		  a : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
		   );
	END  component;
	
	component SIGNED_ADD IS
		generic ( n : integer := 31);
		PORT (din1, din2 : in STD_LOGIC_VECTOR(n downto 0);
			  dout : out STD_LOGIC_VECTOR(n downto 0));
	END component;
    SIGNAL h1 , h2 : integer;
	SIGNAL a , add_out, din1, din2 ,sigout1,sigout2:STD_LOGIC_VECTOR(18 DOWNTO 0);
    SIGNAL p1_index , p2_index: STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL d : std_logic; --direct
	
BEGIN 
        
	comparator : comp_sig
		PORT map ( 
			  din ,  d,  p1_index , p2_index, a
			   );
			   
	h1 <= to_INTEGER(unsigned (p1_index(3 DOWNTO 0)));  
	h2 <= to_INTEGER(unsigned (p2_index(3 DOWNTO 0)));
	din1(17 DOWNTO 0) <=STD_LOGIC_VECTOR( shift_RIGHT(unsigned(din(18 DOWNTO 1)),h1));
	din2(17 DOWNTO 0) <=STD_LOGIC_VECTOR( shift_RIGHT(unsigned(din(18 DOWNTO 1)),h2));
 
    din1(18) <= p1_index(4) xor din(31);
    din2(18) <= p2_index(4) xor din(31);
  
    add1 : SIGNED_ADD 
		generic map (18)
		PORT map (din1, din2 , add_out);
		
	add2 : SIGNED_ADD 
		generic map (18)
		PORT map (add_out, a , sigout2);

	mux1: 	mux_2
	  generic map (18)  
	  PORT map(
			din(15),
			"0100000000000000000" ,"0000000000000000000",
			sigout1
			 );
	mux2:  mux_2
	  generic map (18)
	  PORT map(
			d,
			sigout2,sigout1,
			sigout
			 );
		
		
END behavioral;
---------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;


ENTITY comp_sig IS 
PORT ( 
      signed_din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	  d : OUT std_logic;
	  p1_index , p2_index: OUT STD_LOGIC_VECTOR(4 downto 0);
      a : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
	   );
END comp_sig;


ARCHITECTURE behavioural OF comp_sig IS
signal a_index : std_logic_vector(30 downto 0);
signal a1, m : std_logic_vector(18 downto 0);
signal a2, a2_out : std_logic_vector(18 downto 0);
COMPONENT mux2_31 IS 
	  PORT (
			sel : IN STD_LOGIC;
			a , b : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
			c : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) 
			 );
END COMPONENT;
component SIGNED_ADD IS
		generic ( n : integer := 31);
		PORT (din1, din2 : in STD_LOGIC_VECTOR(n downto 0);
			  dout : out STD_LOGIC_VECTOR(n downto 0));
END component;
component mux_2 is
  generic (
		n : integer := 31
		);
  port (sel : in std_logic;
        a , b : in STD_LOGIC_VECTOR (n DOWNTO 0);
        c :out STD_LOGIC_VECTOR(n DOWNTO 0)  );
end component;
begin 

process(signed_din)
BEGIN 
	d <= '0';
	p1_index <= "00000";
	p2_index <= "00000";
    if ( "000000000000" <  signed_din(30 DOWNTO 19)   )then -- x > 8
      d <= '1';
    elsif ( "11" =  signed_din( 18 DOWNTO 17) )then -- x > 6
			  p1_index <= "01001" ;
			  p2_index <= "11010" ;
			  a_index <= "0000000101111000110110000000000";
    elsif ( "101" =  signed_din(18 DOWNTO 16))then -- x > 5
			  p1_index <= "01000" ;
			  p2_index <= "01111" ;
			  a_index <= "0000001101010011111110000000000";
    elsif ( "100" =  signed_din(18 DOWNTO 16)   )then -- x > 4
			  p1_index <= "00111" ;
			  p2_index <= "01000" ;
			  a_index <= "0000100001010001111010000000000";
    elsif ( "11" =  signed_din(17 DOWNTO 16)   )then -- x > 3
			  p1_index <= "00101" ;
			  p2_index <= "11001" ;
			  a_index <= "0001000101000111101100000000000";
    elsif ( '1' =  signed_din(17)   )then  -- x > 2
			  p1_index <= "00100" ;
			  p2_index <= "00111" ;
			  a_index <= "0010000011000100100110000000000";
    elsif ( "11001100110011001" <  signed_din(16 DOWNTO 0)    )then  -- x > 1.6
			  p1_index <= "00011" ;
			  p2_index <= "11001" ;
			  a_index <= "0010111010010111100100000000000";
    elsif ( "101" = signed_din(16 downto 14)   )then -- x > 1.25
			  p1_index <= "00011" ;
			  p2_index <= "00101" ;
			  a_index <= "0011010110000001000010000000000";
    elsif ( "10011001100110011"  <  signed_din(16 DOWNTO 0)   )then  -- x> 1.2
			  p1_index <= "00011" ;
			  p2_index <= "00100" ;
			  a_index <= "0011101001111110111110000000000";
    elsif ( '1' =  signed_din(16)   )then  -- x> 1
			  p1_index <= "00011" ;
			  p2_index <= "00100" ;
			  a_index <= "0011101001101110100110000000000";
    elsif ( "1110011001100110"  <  signed_din(15 DOWNTO 0)   )then  -- x>0.9
			  p1_index <= "00010" ;
			  p2_index <= "10101" ;
			  a_index <= "0011111001010110000010000000000";
    elsif ( "1100110011001100"  <  signed_din(15 DOWNTO 0)   )then  -- x>0.8
			  p1_index <= "00010" ;
			  p2_index <= "10101" ;
			  a_index <= "0011111000101000001010000000000";
    elsif ( "1001100110011001"  <  signed_din(15 DOWNTO 0)   )then  -- x>0.6
			  p1_index <= "00010" ;
			  p2_index <= "10101" ;
			  a_index <= "0011111000010100011110000000000";
    elsif ( "11001100110011"  <  signed_din(14 DOWNTO 0)   )then  -- x>0.4
			  p1_index <= "00010" ;
			  p2_index <= "10110" ;
			  a_index <= "0011111101011100001010000000000";
    elsif ( '1'  =  signed_din(14)   )then  -- x> 0.25
			  p1_index <= "00010" ;
			  p2_index <= "11000" ;
			  a_index <= "0011111111110010111010000000000";
    elsif ( "10011001100110"  <  signed_din(13 DOWNTO 0)   )then  -- x>0.15
			  p1_index <= "00001" ;
			  p2_index <= "01000" ;
			  a_index <= "0100000000100000110010000000000";
    else
			  p1_index <= "00010" ;
			  p2_index <= "01011" ;
			  a_index <= "0100000000000000000000000000000";
    END if;
  END process;
  
    a1 <= '0'& a_index(30 downto 13);
    m <= "0100000000000000000";
    a2 <= '1'&a_index(30 downto 13);
	add1 : SIGNED_ADD 
		generic map (18)
		PORT map (m, a2 , a2_out);
		
	mux0 :mux_2 
		generic map (18)
		port map(
			signed_din(31) ,
			a1 , a2_out,
			a    );
	
END behavioural;	 
-----------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
use IEEE.numeric_std.all; 

ENTITY SIGNED_ADD IS
generic ( n : integer := 31);
PORT (din1, din2 : in STD_LOGIC_VECTOR(n downto 0);
	  dout : out STD_LOGIC_VECTOR(n downto 0));
END SIGNED_ADD;	  

ARCHITECTURE behavioral OF SIGNED_ADD IS

signal out1, out2, d : std_logic_vector(n-1 downto 0);
signal en , se11, sel2, sign : std_logic;
--signal val : std_logic_vector(31 downto 0):= (others =>'0');
signal do : std_logic_vector(n-1 downto 0);


component twoscompliment IS
generic ( n : integer := 31);
PORT (en : in std_logic;
      din : in STD_LOGIC_VECTOR(n downto 0);
	    dout : out STD_LOGIC_VECTOR(n downto 0));
END component;

component mux_2 is
  generic (
		n : integer := 31
		);
  port (
        sel : in std_logic;
        a , b : in STD_LOGIC_VECTOR (n DOWNTO 0);
        c :out STD_LOGIC_VECTOR(n DOWNTO 0) 
         );
end component;

BEGIN
    
process(din1,din2)
 begin
  
    if ( din1(n) = din2(n) ) then
       en <= '0';
       se11 <= '0'; -- din1
       sel2 <= '1'; -- din2
       sign <= din1(n);
    ELSif (din1(n-1 downto 0) > din2(n-1 downto 0)) then
       en <= '1';
       se11 <= '0'; -- din1
       sel2 <= '1'; -- din2
       sign <= din1(n);
    else
       en <= '1';
       se11 <= '1'; -- din1
       sel2 <= '0'; -- din2
       sign <= din2(n);
    end if;
	
 end process;
 
 comp : twoscompliment 
	generic map (n-1)
    PORT MAP(en ,
      out2 ,
	    d);
	    
  mux0 :mux_2 
  generic map (n-1)
  port map(
        se11 ,
        din1(n-1 downto 0) , din2(n-1 downto 0),
        out1
         );
  mux1 :mux_2 
  generic map (n-1)
  port map(
        sel2 ,
        din1(n-1 downto 0) , din2(n-1 downto 0),
        out2
         );      
  do <= STD_LOGIC_VECTOR (UNSIGNED(out1) + UNSIGNED(d));
  dout  <= sign & do;
  
END behavioral;

------------------------------------------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
use IEEE.numeric_std.all; 

ENTITY twoscompliment IS
generic ( n : integer := 31);
PORT (en : in std_logic;
      din : in STD_LOGIC_VECTOR(n downto 0);
	    dout : out STD_LOGIC_VECTOR(n downto 0));
END twoscompliment;	  

architecture behavioural1 of twoscompliment is

begin
  process ( en , din )
    begin
      if (en ='1') then
          dout <= std_logic_vector(unsigned(not din )+ 1); -- + "00000000000000000000000000000001");
      else
          dout <= din;
      end if;
   end process;
end behavioural1;
------------------------------------------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;


entity mux_2 is
  generic ( n : integer := 31);
  port (
        sel : in std_logic;
        a , b : in STD_LOGIC_VECTOR (n DOWNTO 0);
        c :out STD_LOGIC_VECTOR(n DOWNTO 0) 
         );
end mux_2;

architecture behavioural1 of mux_2 is 
begin 
 
 process(sel , a , b)
	 begin
	  case sel is 
	    when '0' => c <= a ;
	    when '1' => c <= b ;
	    when others => c <= (others =>'0'); 
	  end case;
end process;  

END behavioural1;



------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY test_activation IS 
END test_activation;	  

ARCHITECTURE tb OF test_activation IS 

	COMPONENT tanh IS 
		PORT ( 
			  din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
			  tanhout : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
			   );
	END COMPONENT;
	COMPONENT sigmoid IS 
		PORT ( 
			  din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
			  sigout : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
			   );
	END COMPONENT;	
signal din :  STD_LOGIC_VECTOR(31 DOWNTO 0);
signal tanhout, sigout :  STD_LOGIC_VECTOR (18 DOWNTO 0);

begin 
 
din <=  "10000000010000000010000001000000" , 			 "10000000000001000010000001000000"  after 10 ns,"10000000000000100010000001000000"  after 20 ns,"10000000000000010010000001000000"  after 30 ns,
		"10000000000000011001100110011101"  after 40 ns,"10000000000000010011001110110011"  after 50 ns,"10000000000000001010000001000000"  after 60 ns,"10000000000000010010000001000000"  after 70 ns,
		"10000000000000001110011001100110"  after 80 ns,"10000000000000001100110011001100"  after 90 ns,"10000000000000001001100110011001"  after 100 ns,"10000000000000000011001100110011"  after 110 ns,
		"10000000000000000010011001100110"  after 120 ns,"10000000000000000000000001000000"  after 130 ns,"10000000000000000010000001000000"  after 140 ns,"10000000000000001011110101110000"  after 150 ns,
		"00000000010000000010000001000000" after 200 ns, "00000000000001000010000001000000"  after 210 ns,"00000000000000100010000001000000"  after 220 ns,"00000000000000010010000001000000"  after 230 ns,
		"00000000000000011001100110011101"  after 240 ns,"00000000000000010011001110110011"  after 250 ns,"00000000000000001010000001000000"  after 260 ns,"00000000000000010010000001000000"  after 270 ns,
		"00000000000000001110011001100110"  after 280 ns,"00000000000000001100110011001100"  after 290 ns,"00000000000000001001100110011001"  after 300 ns,"00000000000000000011001100110011"  after 310 ns,
		"00000000000000000010011001100110"  after 320 ns,"00000000000000000000000001000000"  after 330 ns,"00000000000000000010000001000000"  after 340 ns,"00000000000000001011110101110000"  after 350 ns;
tanh1 : tanh  
		PORT map ( 
			  din ,tanhout
			   );
sigmoid1 : sigmoid  
		PORT map ( 
			  din ,sigout
			   );

END tb;
