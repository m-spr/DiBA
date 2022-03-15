------- element-wise

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY AAP IS  
  PORT(
    clk, rst, run: IN STD_LOGIC;
    dmap: IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	reg_out : OUT STD_LOGIC;
	hout : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
  );
END AAP;

ARCHITECTURE behavioral OF AAP IS 
 

COMPONENT p3 IS  
  PORT(
    h_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    d : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    pointer_c : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    count_add : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    clk , count_tanh, count_activation , count_mul,  en_c , reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c: IN STD_LOGIC  
  );
END COMPONENT;

COMPONENT AAP_controller IS  
  PORT(clk ,rst, run : IN STD_LOGIC;
	reg_out : OUT STD_LOGIC ; 
    pointer_c_out : OUT STD_LOGIC_VECTOR(9 DOWNTO 0);
    count_add : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    count_tanh, count_activation , count_mul : OUT STD_LOGIC ;
    en_c ,reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c : OUT STD_LOGIC
   );
END COMPONENT;

SIGNAL h_out :  STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL pointer_c : STD_LOGIC_VECTOR(9 DOWNTO 0);
SIGNAL count_add :  STD_LOGIC_VECTOR(1 DOWNTO 0);
SIGNAL count_tanh, count_activation , count_mul, en_c ,   reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c: STD_LOGIC ; 
 
BEGIN 
    
    
p : p3
  PORT MAP(
    h_out,
    dmap , 
    pointer_c ,
    count_add ,
    clk , count_tanh, count_activation , count_mul, en_c ,  reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c
  );
  

p_c : AAP_controller  
  PORT MAP(clk ,rst, run ,
    reg_out,
	pointer_c ,
    count_add ,
    count_tanh, count_activation , count_mul,
    en_c ,reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c
  );
  
  
  process (h_out)
  BEGIN  
  hout(15) <= h_out (31);
	if (h_out (31) ='1') then 
		hout(14 DOWNTO 0) <= STD_LOGIC_VECTOR(signed(not(h_out(30 DOWNTO 16)))+1);
	else
		hout (14 DOWNTO 0) <= h_out (30 DOWNTO 16);
	END if; 
   END process; 
    
END behavioral;  

--------------------------------------------------------------------------------------------------------------------------------------


LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY p3 IS  
  PORT(
    h_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    d : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    pointer_c : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    count_add : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    clk , count_tanh, count_activation , count_mul, en_c ,  reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c: IN STD_LOGIC  
  );
END p3;

ARCHITECTURE behavioral OF p3 IS 

COMPONENT single_port_ram_aap IS
  GENERIC ( n : INteger := 15;
			addr_width : INteger := 4 ;
			row : INteger := 4 
			);
  PORT(
    clk: IN STD_LOGIC;
    we : IN STD_LOGIC;
    addr : IN STD_LOGIC_VECTOR(addr_width DOWNTO 0);
    din : IN STD_LOGIC_VECTOR(n DOWNTO 0);
    dout : OUT STD_LOGIC_VECTOR(n DOWNTO 0)
    );
END COMPONENT;

COMPONENT mux2_31 IS 
  PORT (
        sel : IN STD_LOGIC;
        a , b : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
        c : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) 
         );
END COMPONENT;

COMPONENT reg IS 
PORT (clk , en : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	    dout : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
END COMPONENT;
	
COMPONENT demux IS 
  PORT (
        sel : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        a , b ,c : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
        d : IN STD_LOGIC_VECTOR(31 DOWNTO 0) 
         );
END COMPONENT;
 
COMPONENT tanh IS 
PORT ( 
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      tanhout : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	   );
END COMPONENT;	
  
COMPONENT sigmoid IS 
PORT ( 
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      sigout : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	   );
END COMPONENT;	 
-------------***********************************************************************----------------------
--TYPE c_mem IS  array (0 TO 127) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
-------------***********************************************************************----------------------
--SIGNAL c_ram : c_mem := (others=>(others=>'0'));
SIGNAL sig_out, sig_in , reg_sig,tan_in,tan_in_1, tan_out, reg_tan, mull_in, reg_mull, demux_c, c_reg, demux_add  ,add_out, reg_addo : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL mull_out : STD_LOGIC_VECTOR(62 DOWNTO 0);
SIGNAL c : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL extra :STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL demux_activation : STD_LOGIC_VECTOR(1 DOWNTO 0);
SIGNAL mull_out_reg :STD_LOGIC_VECTOR(63 DOWNTO 0);
BEGIN 
  
   C_RAM : single_port_ram_aap
  GENERIC MAP( 18,
			addr_col ,
			col-1
			)
  PORT MAP(
    clk, en_c, pointer_c , demux_c, c   );
  
  --process (clk)
  --  BEGIN 
  --if (rising_edge(clk)) then
  --  if (en_c = '1')then
--		c_ram(to_INTEGER(unsigned(pointer_c))) <=   demux_c ;
--	else
--		c <=  c_ram(to_INTEGER(unsigned(pointer_c)));
--	END if;
-- END if;
 -- END process;
  
  
  demux_activation <= '0'& count_activation;
  
  demux_input : demux 
  PORT MAP( demux_activation, sig_in , tan_in_1 ,extra, d  );
		
  sig : sigmoid
  PORT MAP (    sig_in, sig_out );

  mux2_1 : mux2_31
  PORT MAP ( count_tanh ,tan_in_1 , reg_addo , tan_in  );

  tan : tanh 
  PORT MAP(  tan_in , tan_out  );
    
  reg_sig_m : reg 
  PORT MAP ( clk , reg_en_sig, sig_out , reg_sig );  
  
  reg_tan_m : reg 
  PORT MAP ( clk , reg_en_tan, tan_out , reg_tan );
  
  mux3_1 : mux2_31
  PORT MAP ( count_mul ,reg_tan ,c , mull_in  );
  
  mull_out_reg <= STD_LOGIC_VECTOR(signed(mull_in)*signed(reg_sig));
  --mull_out(62) <= mull_in(31) xor reg_sig(31); 
  --mull_out_reg <= mull_out(62)&mull_out(60 DOWNTO 30);
  
  reg_mull_m : reg 
  PORT MAP ( clk , reg_en_mull, mull_out_reg(63 downto 32) , reg_mull );

  add_demux : demux 
  PORT MAP(  count_add, demux_c, demux_add, h_out , reg_mull);
   
  reg_demux_m : reg 
  PORT MAP ( clk , reg_en_demux_c , demux_c , c_reg );  
        
  add_out <=  STD_LOGIC_VECTOR(signed(c_reg) + signed (demux_add)) ;
  
  reg_add_m : reg 
  PORT MAP ( clk , reg_en_add_out, add_out , reg_addo );
 
    
    
END behavioral;	 
-----------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY AAP_controller IS  
  PORT(clk ,rst, run : IN STD_LOGIC;
	reg_out : OUT STD_LOGIC ; 
    pointer_c_out : OUT STD_LOGIC_VECTOR(9 DOWNTO 0);
    count_add : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    count_tanh, count_activation , count_mul : OUT STD_LOGIC ;
    en_c ,reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c : OUT STD_LOGIC
  );
END AAP_controller;

ARCHITECTURE ctrl OF AAP_controller IS 

TYPE state IS  (s0,s1,s1i,s2,s2i,s3,s3i,s4,s4i,s1_1,s1_2,s1_3,s2_1,s3_1,s3_2,s3_3,s4_1,s4_2,s4_3);
SIGNAL ps , ns : state;
SIGNAL pointer_en ,pointer_rst : STD_LOGIC;
SIGNAL pointer: STD_LOGIC_VECTOR(9 DOWNTO 0);
COMPONENT counter IS 
  GENERIC ( n : INTEGER := 8 );
  PORT (clk ,rst , en : IN STD_LOGIC;
        c : OUT STD_LOGIC_VECTOR(n DOWNTO 0) 
         );
END COMPONENT;
BEGIN 
  
  process(clk)
    BEGIN 
    if (rising_edge(clk)) then

    if (rst = '1')then
      ps <= s0; 
    else  
      ps <= ns;  
    END if;
  END if;
  END process;
  process (ps , run)
  BEGIN 
  case (ps) IS 
   when s0 => 
            if (run ='1') then
              ns <= s1_1;
            else
              ns <= s0;
            END if;
   when s1 => if (run ='1') then
              ns <= s1_1;
            else
              ns <= s1;
            END if;
   when s1_1 => ns <= s1_2;
   when s1_2 => ns <= s1_3;
   when s1_3 => ns <= s1i;
   when s1i =>
			if (run ='1') then
              ns <= s2;
            else
              ns <= s1i;
            END if;
   when s2 => ns <= s2_1;
   when s2_1 => ns <= s2i;
   when s2i =>
			if (run ='1') then
              ns <= s3;
            else
              ns <= s2i;
            END if;
   when s3 => ns <= s3_1;
   when s3_1 => ns <= s3_2;
   when s3_2 => ns <= s3_3;
   when s3_3 => ns <= s3i;
   when s3i =>
			if (run ='1') then
              ns <= s4;
            else
              ns <= s3i;
            END if;
   when s4 => ns <= s4_1;
   when s4_1 => ns <= s4_2;
   when s4_2 => ns <= s4_3;
   when s4_3 => ns <= s4i;
   when s4i =>
			if (run ='1') then
              ns <= s1_1;
            else
              ns <= s4i;
            END if;
   
  when others => ns <= s0;
  END case;
END process;

 count : counter 
  GENERIC MAP ( 9 )
  PORT MAP (clk ,pointer_rst , pointer_en , pointer );


process(ps, pointer)


BEGIN 

  count_add <="00";
  count_tanh <='0';
  count_activation <='0';
  count_mul <='0';
  reg_en_sig <='0';
  en_c <= '0';
  reg_en_tan <='0';
  reg_en_mull <='0';
  reg_en_add_in <='0';
  reg_en_add_out <='0';
  reg_en_demux_c <='0';
  reg_out <= '0';
  pointer_rst <= '0'; 
  pointer_en <= '0';

  case (ps) IS 
   when s0 =>   pointer_rst <= '1';
   
   when s1_1 =>
				count_activation <= '0';
   when s1_2 => reg_en_sig <= '1';
				count_mul <= '1';
				if (pointer = "0000110010") then 
					pointer_rst <= '1';
				end if;
   when s1_3 => count_mul <= '1';
				reg_en_mull <= '1';
				count_add <= "00";
				reg_en_demux_c <='1';
				reg_en_add_out <='1';
   when s2 => count_add <= "00";
				reg_en_add_out <='1';
				reg_en_demux_c <='1';
				count_activation <= '0';
   when s2_1 => reg_en_sig <= '1';  
   when s3 => count_activation <= '1';
              count_tanh <= '0';
   when s3_1 => reg_en_tan <= '1';
				count_mul <= '0';
   when s3_2 => reg_en_mull <= '1';
				count_add <= "01";
   when s3_3 => count_add <= "01";
				reg_en_add_out <='1';
   when s4 => count_activation <= '0';
				count_tanh <= '1';
				en_c <= '1';
   when s4_1 => reg_en_sig <= '1';
				reg_en_tan <= '1';
   when s4_2 => reg_en_mull <= '1';
					
   when s4_3 => count_add <= "11";
				pointer_en <= '1';
				reg_out <= '1';
  when others => count_add <="00";
                count_tanh <='0';
                count_activation <='0';
				en_c <= '0';
                count_mul <='0';
                reg_en_sig <='0';
                reg_en_tan <='0';
                reg_en_mull <='0';
                reg_en_add_in <='0';
                reg_en_add_out <='0';
                reg_en_demux_c <='0';
				reg_out <= '0';
				pointer_en <= '0';
				pointer_rst <= '0';
  END case;
  END process;
    
    pointer_c_out <= pointer;
END ctrl;  

------------------------------------------------------------------------------------------------
----------------------------------------------------------
LIBRARY ieee;
USE ieee.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL; 

ENTITY reg IS
GENERIC (n : INteger := 31);
PORT (clk , en : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(n DOWNTO 0);
	    dout : OUT STD_LOGIC_VECTOR(n DOWNTO 0));
END reg;	  

ARCHITECTURE behavioural1 OF reg IS
BEGIN
PROCESS(clk)
 BEGIN
  
    IF  rISINg_edge(clk) THEN
      IF(en = '1') THEN
       dout <= din;
     END IF;
    END IF;
    
 END PROCESS;
END behavioural1;	
------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY tanh IS 
PORT ( 
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      tanhout : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
	   );
END tanh;	  

ARCHITECTURE behavioral OF tanh IS 

	COMPONENT mux_2 IS
	  GENERIC ( n : INteger := 31);
	  PORT (
			sel : IN STD_LOGIC;
			a , b : IN STD_LOGIC_VECTOR (n DOWNTO 0);
			c :OUT STD_LOGIC_VECTOR(n DOWNTO 0) 
			 );
	END COMPONENT;
	
	COMPONENT comp_tanh IS 
		PORT ( 
			  signed_din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
			  d : OUT STD_LOGIC;
			  p1_INdex , p2_INdex: OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
			  a : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
			   );
	END COMPONENT;
	
	COMPONENT SIGNED_ADD IS
		GENERIC ( n : INteger := 31);
		PORT (din1, din2 : IN STD_LOGIC_VECTOR(n DOWNTO 0);
			  dout : OUT STD_LOGIC_VECTOR(n DOWNTO 0));
	END COMPONENT;

	SIGNAL a , add_out, din1, din2 , tanhout1, tanhout2:STD_LOGIC_VECTOR(18 DOWNTO 0);
    SIGNAL p1_INdex , p2_INdex: STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL d : STD_LOGIC; --direct
	SIGNAL h1 , h2 : INteger;
BEGIN 
        
	comparaTOr : comp_tanh 
		PORT MAP ( 
			  din ,  d,  p1_INdex , p2_INdex, a
			   );
	h1 <= TO_INTEGER(unsigned (p1_INdex(3 DOWNTO 0)));
	h2 <= TO_INTEGER(unsigned (p2_INdex(3 DOWNTO 0)));

    din1(17 DOWNTO 0) <=STD_LOGIC_VECTOR( shIFt_RIGHT(unsigned(din(17 DOWNTO 0)),h1));
	din2(17 DOWNTO 0) <=STD_LOGIC_VECTOR( shIFt_RIGHT(unsigned(din(17 DOWNTO 0)),h2));
  
    din1(18) <= p1_INdex(4) xor din(31);
    din2(18) <= p2_INdex(4) xor din(31);
  
    add1 : SIGNED_ADD 
		GENERIC MAP (18)
		PORT MAP (din1, din2 , add_out);
		
	add2 : SIGNED_ADD 
		GENERIC MAP (18)
		PORT MAP (add_out, a , tanhout1);
		
	mux1: 	mux_2
	  GENERIC MAP (18)  
	  PORT MAP(
			din(31),
			"0100000000000000000" ,"1100000000000000000",
			tanhout2
			 );
	mux2:  mux_2
	  GENERIC MAP (18)  
	  PORT MAP(
			d,
			tanhout1,tanhout2,
			tanhout
			 );
		
END behavioral;	 
---------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;


ENTITY comp_tanh IS 
PORT ( 
      signed_din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	  d : OUT STD_LOGIC;
	  p1_INdex , p2_INdex: OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
      a : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
	   );
END comp_tanh;


ARCHITECTURE behavioural OF comp_tanh IS
SIGNAL a_INdex : STD_LOGIC_VECTOR(30 DOWNTO 0);
BEGIN 

PROCESS(signed_din)
BEGIN 
d <= '0';
p1_INdex <= "00000";
p2_INdex <= "00000";

    IF (  signed_din(30 DOWNTO 18)  > "0000000000000" )THEN                -- x > 4
			  d <= '1';
    elsIF ( "101110011001100110" =  signed_din(17 DOWNTO 0)  )THEN  -- x > 2.9
			  p1_INdex <= "01000" ;
			  p2_INdex <= "01010" ;
			  a_INdex <= "0111110110110010001100000000000";
    elsIF ( "101" =  signed_din(17 DOWNTO 15)   )THEN                -- x > 2.5
			  p1_INdex <= "00110" ;
			  p2_INdex <= "01000" ;
			  a_INdex <= "0111100000010000011000000000000";
    elsIF ( '1'  = signed_din(17)   )THEN                             -- x > 2
			  p1_INdex <= "00100" ;
			  p2_INdex <= "10110" ;
			  a_INdex <= "0110111101111100111100000000000";  
    elsIF ( "11011100001010001" =  signed_din(16 DOWNTO 0)   )THEN  -- x > 1.72
			  p1_INdex <= "00100" ;
			  p2_INdex <= "00101" ;
			  a_INdex <= "0110001110000101001000000000000";  
    elsIF ( "11000010100011110" <  signed_din(16 DOWNTO 0)  )THEN  -- x > 1.52
			  p1_INdex <= "00010" ;
			  p2_INdex <= "00110" ;
			  a_INdex <= "0101100100010110100010000000000";
    elsIF ( "10100011110101110"  <  signed_din(16 DOWNTO 0) )THEN   -- x > 1.28
			  p1_INdex <= "00010" ;
			  p2_INdex <= "10101" ;
			  a_INdex <= "0100100111101011100010000000000";
    elsIF ( "10001100110011001"  <  signed_din(16 DOWNTO 0)   )THEN  -- x> 1.1
			  p1_INdex <= "00010" ;
			  p2_INdex <= "00100" ;
			  a_INdex <="0011101010001111011000000000000" ;
    elsIF ( "1101110000101000" <  signed_din(15 DOWNTO 0)    )THEN -- x> 0.86
			  p1_INdex <= "00001" ;
			  p2_INdex <= "10100" ;
			  a_INdex <= "0010100100110000110000000000000";
    elsIF ( "1011110101110000" <  signed_din(15 DOWNTO 0)    )THEN -- x > 0.74
			  p1_INdex <= "00001" ;
			  p2_INdex <= "00100" ;
			  a_INdex <= "0001101101001011001000000000000";
    elsIF ( "1010000101000111" <  signed_din(15 DOWNTO 0)    )THEN -- x > 0.63
			  p1_INdex <= "00001" ;
			  p2_INdex <= "00010" ;
			  a_INdex <= "0001010100111111100000000000000";
    elsIF ( "111101011100001" <  signed_din(14 DOWNTO 0)    )THEN -- x > 0.48
			  p1_INdex <= "00001" ;
			  p2_INdex <= "00010" ;
			  a_INdex <= "0000101100110011001100000000000";
	elsIF ( "111000010100011" <  signed_din(14 DOWNTO 0)    )THEN -- x > 0.44
			  p1_INdex <= "00000" ;
			  p2_INdex <= "10010" ;
	          a_INdex <= "0000001110001000011010000000000";
    elsIF ( "101000111101011" <  signed_din(14 DOWNTO 0)    )THEN -- x > 0.32
			  p1_INdex <= "00000" ;
			  p2_INdex <= "10010" ;
			  a_INdex <= "0000001111010000100000000000000";
    elsIF ( '1' =  signed_din(14)    )THEN                        -- x > 0.25
			  p1_INdex <= "00000" ;
			  p2_INdex <= "10100" ;
			  a_INdex <= "0000000101000100011010000000000";
    elsIF ( "10011001100110" <  signed_din(13 DOWNTO 0)    )THEN -- x > 0.15
			  p1_INdex <= "00000" ;
			  p2_INdex <= "10101" ;
			  a_INdex <= "0000000001100101100110000000000";		
    ELSE
			  p1_INdex <= "00001" ;
			  p2_INdex <= "00001" ;
			  a_INdex <=  "0000000000000011010010000000000" ;
      
    END IF;
  END PROCESS;
  
    a <= signed_din(31)& a_INdex(30 DOWNTO 13);

  
END behavioural;	 
------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY reg IS 
GENERIC (n : INTEGER);
PORT (clk , en : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(n DOWNTO 0);
	    dout : OUT STD_LOGIC_VECTOR(n DOWNTO 0));
END reg;	  

ARCHITECTURE behavioral OF reg IS 
BEGIN 
PROCESS(clk)
 BEGIN 
  
    IF  rISINg_edge(clk) THEN
      IF(en = '1') THEN
       dout <= din;
     END IF;
    END IF;
    
 END PROCESS;
END behavioral;	 
------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY sigmoid IS 
PORT ( 
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      sigOUT : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
	   );
END sigmoid;	  

ARCHITECTURE behavioral OF sigmoid IS 


	COMPONENT  mux_2 IS
	  GENERIC ( n : INteger := 31);
	  PORT (
			sel : IN STD_LOGIC;
			a , b : IN STD_LOGIC_VECTOR (n DOWNTO 0);
			c :OUT STD_LOGIC_VECTOR(n DOWNTO 0) 
			 );
	END COMPONENT;
	
	COMPONENT comp_sig IS 
	PORT ( 
		  signed_din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
		  d : OUT STD_LOGIC;
		  p1_INdex , p2_INdex: OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
		  a : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
		   );
	END  COMPONENT;
	
	COMPONENT SIGNED_ADD IS
		GENERIC ( n : INteger := 31);
		PORT (din1, din2 : IN STD_LOGIC_VECTOR(n DOWNTO 0);
			  dout : OUT STD_LOGIC_VECTOR(n DOWNTO 0));
	END COMPONENT;
    SIGNAL h1 , h2 : INteger;
	SIGNAL a , add_out, din1, din2 ,sigout1,sigout2:STD_LOGIC_VECTOR(18 DOWNTO 0);
    SIGNAL p1_INdex , p2_INdex: STD_LOGIC_VECTOR(4 DOWNTO 0);
	SIGNAL d : STD_LOGIC; --direct
	
BEGIN 
        
	comparaTOr : comp_sig
		PORT MAP ( 
			  din ,  d,  p1_INdex , p2_INdex, a
			   );
			   
	h1 <= TO_INTEGER(unsigned (p1_INdex(3 DOWNTO 0)));  
	h2 <= TO_INTEGER(unsigned (p2_INdex(3 DOWNTO 0)));
	din1(17 DOWNTO 0) <=STD_LOGIC_VECTOR( shIFt_RIGHT(unsigned(din(18 DOWNTO 1)),h1));
	din2(17 DOWNTO 0) <=STD_LOGIC_VECTOR( shIFt_RIGHT(unsigned(din(18 DOWNTO 1)),h2));
 
    din1(18) <= p1_INdex(4) xor din(31);
    din2(18) <= p2_INdex(4) xor din(31);
  
    add1 : SIGNED_ADD 
		GENERIC MAP (18)
		PORT MAP (din1, din2 , add_out);
		
	add2 : SIGNED_ADD 
		GENERIC MAP (18)
		PORT MAP (add_out, a , sigout2);

	mux1: 	mux_2
	  GENERIC MAP (18)  
	  PORT MAP(
			din(15),
			"0100000000000000000" ,"0000000000000000000",
			sigout1
			 );
	mux2:  mux_2
	  GENERIC MAP (18)
	  PORT MAP(
			d,
			sigout2,sigout1,
			sigout
			 );
		
		
END behavioral;
---------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;


ENTITY comp_sig IS 
PORT ( 
      signed_din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	  d : OUT STD_LOGIC;
	  p1_INdex , p2_INdex: OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
      a : OUT STD_LOGIC_VECTOR (18 DOWNTO 0)
	   );
END comp_sig;


ARCHITECTURE behavioural OF comp_sig IS
SIGNAL a_INdex : STD_LOGIC_VECTOR(30 DOWNTO 0);
SIGNAL a1, m : STD_LOGIC_VECTOR(18 DOWNTO 0);
SIGNAL a2, a2_OUT : STD_LOGIC_VECTOR(18 DOWNTO 0);

COMPONENT SIGNED_ADD IS
		GENERIC ( n : INteger := 31);
		PORT (din1, din2 : IN STD_LOGIC_VECTOR(n DOWNTO 0);
			  dout : OUT STD_LOGIC_VECTOR(n DOWNTO 0));
END COMPONENT;
COMPONENT mux_2 IS
  GENERIC (
		n : INteger := 31
		);
  PORT (sel : IN STD_LOGIC;
        a , b : IN STD_LOGIC_VECTOR (n DOWNTO 0);
        c :OUT STD_LOGIC_VECTOR(n DOWNTO 0)  );
END COMPONENT;
BEGIN 

PROCESS(signed_din)
BEGIN 
	d <= '0';
	p1_INdex <= "00000";
	p2_INdex <= "00000";
    IF ( "000000000000" <  signed_din(30 DOWNTO 19)   )THEN -- x > 8
      d <= '1';
    elsIF ( "11" =  signed_din( 18 DOWNTO 17) )THEN -- x > 6
			  p1_INdex <= "01001" ;
			  p2_INdex <= "11010" ;
			  a_INdex <= "0000000101111000110110000000000";
    elsIF ( "101" =  signed_din(18 DOWNTO 16))THEN -- x > 5
			  p1_INdex <= "01000" ;
			  p2_INdex <= "01111" ;
			  a_INdex <= "0000001101010011111110000000000";
    elsIF ( "100" =  signed_din(18 DOWNTO 16)   )THEN -- x > 4
			  p1_INdex <= "00111" ;
			  p2_INdex <= "01000" ;
			  a_INdex <= "0000100001010001111010000000000";
    elsIF ( "11" =  signed_din(17 DOWNTO 16)   )THEN -- x > 3
			  p1_INdex <= "00101" ;
			  p2_INdex <= "11001" ;
			  a_INdex <= "0001000101000111101100000000000";
    elsIF ( '1' =  signed_din(17)   )THEN  -- x > 2
			  p1_INdex <= "00100" ;
			  p2_INdex <= "00111" ;
			  a_INdex <= "0010000011000100100110000000000";
    elsIF ( "11001100110011001" <  signed_din(16 DOWNTO 0)    )THEN  -- x > 1.6
			  p1_INdex <= "00011" ;
			  p2_INdex <= "11001" ;
			  a_INdex <= "0010111010010111100100000000000";
    elsIF ( "101" = signed_din(16 DOWNTO 14)   )THEN -- x > 1.25
			  p1_INdex <= "00011" ;
			  p2_INdex <= "00101" ;
			  a_INdex <= "0011010110000001000010000000000";
    elsIF ( "10011001100110011"  <  signed_din(16 DOWNTO 0)   )THEN  -- x> 1.2
			  p1_INdex <= "00011" ;
			  p2_INdex <= "00100" ;
			  a_INdex <= "0011101001111110111110000000000";
    elsIF ( '1' =  signed_din(16)   )THEN  -- x> 1
			  p1_INdex <= "00011" ;
			  p2_INdex <= "00100" ;
			  a_INdex <= "0011101001101110100110000000000";
    elsIF ( "1110011001100110"  <  signed_din(15 DOWNTO 0)   )THEN  -- x>0.9
			  p1_INdex <= "00010" ;
			  p2_INdex <= "10101" ;
			  a_INdex <= "0011111001010110000010000000000";
    elsIF ( "1100110011001100"  <  signed_din(15 DOWNTO 0)   )THEN  -- x>0.8
			  p1_INdex <= "00010" ;
			  p2_INdex <= "10101" ;
			  a_INdex <= "0011111000101000001010000000000";
    elsIF ( "1001100110011001"  <  signed_din(15 DOWNTO 0)   )THEN  -- x>0.6
			  p1_INdex <= "00010" ;
			  p2_INdex <= "10101" ;
			  a_INdex <= "0011111000010100011110000000000";
    elsIF ( "11001100110011"  <  signed_din(14 DOWNTO 0)   )THEN  -- x>0.4
			  p1_INdex <= "00010" ;
			  p2_INdex <= "10110" ;
			  a_INdex <= "0011111101011100001010000000000";
    elsIF ( '1'  =  signed_din(14)   )THEN  -- x> 0.25
			  p1_INdex <= "00010" ;
			  p2_INdex <= "11000" ;
			  a_INdex <= "0011111111110010111010000000000";
    elsIF ( "10011001100110"  <  signed_din(13 DOWNTO 0)   )THEN  -- x>0.15
			  p1_INdex <= "00001" ;
			  p2_INdex <= "01000" ;
			  a_INdex <= "0100000000100000110010000000000";
    ELSE
			  p1_INdex <= "00010" ;
			  p2_INdex <= "01011" ;
			  a_INdex <= "0100000000000000000000000000000";
    END IF;
  END PROCESS;
  
    a1 <= '0'& a_INdex(30 DOWNTO 13);
    m <= "0100000000000000000";
    a2 <= '1'&a_INdex(30 DOWNTO 13);
	add1 : SIGNED_ADD 
		GENERIC MAP (18)
		PORT MAP (m, a2 , a2_out);
		
	mux0 :mux_2 
		GENERIC MAP (18)
		PORT MAP(
			signed_din(31) ,
			a1 , a2_out,
			a    );
	
END behavioural;	 
-----------------------------------------------------------------
LIBRARY ieee;
USE ieee.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL; 

ENTITY SIGNED_ADD IS
GENERIC ( n : INteger := 31);
PORT (din1, din2 : IN STD_LOGIC_VECTOR(n DOWNTO 0);
	  dout : OUT STD_LOGIC_VECTOR(n DOWNTO 0));
END SIGNED_ADD;	  

ARCHITECTURE behavioral OF SIGNED_ADD IS


 
signal out1, out2, d : std_logic_vecTOr(n-1 DOWNTO 0);
signal en , se11, sel2, sign : std_logic;
--signal val : std_logic_vecTOr(31 DOWNTO 0):= (others =>'0');
signal do : std_logic_vecTOr(n-1 DOWNTO 0);


component twoscompliment IS
GENERIC ( n : integer := 31);
PORT (en : in std_logic;
      din : in STD_LOGIC_VECTOR(n DOWNTO 0);
	    dout : out STD_LOGIC_VECTOR(n DOWNTO 0));
END component;

component mux_2 is
  GENERIC (
		n : integer := 31
		);
  PORT (
        sel : in std_logic;
        a , b : in STD_LOGIC_VECTOR (n DOWNTO 0);
        c :out STD_LOGIC_VECTOR(n DOWNTO 0) 
         );
END component;

BEGIN
    
PROCESS(din1,din2)
 BEGIN
  
    IF ( din1(n) = din2(n) ) THEN
       en <= '0';
       se11 <= '0'; -- din1
       sel2 <= '1'; -- din2
       sign <= din1(n);
    ELSIF (din1(n-1 DOWNTO 0) > din2(n-1 DOWNTO 0)) THEN
       en <= '1';
       se11 <= '0'; -- din1
       sel2 <= '1'; -- din2
       sign <= din1(n);
    ELSE
       en <= '1';
       se11 <= '1'; -- din1
       sel2 <= '0'; -- din2
       sign <= din2(n);
    END IF;
	
 END PROCESS;
 
 comp : twoscompliment 
	GENERIC MAP (n-1)
    PORT MAP(en ,
      out2 ,
	    d);
	    
  mux0 :mux_2 
  GENERIC MAP (n-1)
  PORT MAP(
        se11 ,
        din1(n-1 DOWNTO 0) , din2(n-1 DOWNTO 0),
        out1
         );
  mux1 :mux_2 
  GENERIC MAP (n-1)
  PORT MAP(
        sel2 ,
        din1(n-1 DOWNTO 0) , din2(n-1 DOWNTO 0),
        out2
         );      
  do <= STD_LOGIC_VECTOR (UNSIGNED(out1) + UNSIGNED(d));
  dout  <= sign & do;
   
END behavioral;

------------------------------------------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL; 

ENTITY twoscompliment IS
GENERIC ( n : INteger := 31);
PORT (en : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(n DOWNTO 0);
	    dout : OUT STD_LOGIC_VECTOR(n DOWNTO 0));
END twoscompliment;	  

ARCHITECTURE behavioural1 OF twoscompliment IS

BEGIN
  PROCESS ( en , din )
    BEGIN
      IF (en ='1') THEN
          dout <= STD_LOGIC_VECTOR(unsigned(not din )+ 1);
      ELSE
          dout <= din;
      END IF;
   END PROCESS;
END behavioural1;
------------------------------------------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;


ENTITY mux_2 IS
  GENERIC ( n : INteger := 31);
  PORT (
        sel : IN STD_LOGIC;
        a , b : IN STD_LOGIC_VECTOR (n DOWNTO 0);
        c :OUT STD_LOGIC_VECTOR(n DOWNTO 0) 
         );
END mux_2;

ARCHITECTURE behavioural1 OF mux_2 IS 
BEGIN 
 
 PROCESS(sel , a , b)
	 BEGIN
	  case sel IS 
	    WHEN '0' => c <= a ;
	    WHEN '1' => c <= b ;
	    WHEN OTHERS => c <= (OTHERS =>'0'); 
	  END case;
END PROCESS;  

END behavioural1;


-----------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;


ENTITY mux2_31 IS 
  PORT (
        sel : IN STD_LOGIC;
        a , b : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
        c : OUT STD_LOGIC_VECTOR(31 DOWNTO 0) 
         );
END mux2_31;

architecture behavioral1 OF mux2_31 IS  
BEGIN  
 
 process(sel , a , b)
	 BEGIN 
	  case sel IS  
	    when '0' => c <= a ;
	    when '1' => c <= b ;
	    when others => c <= (others =>'0'); 
	  END case;
END process;  

END behavioral1;
------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;


ENTITY counter IS 
  GENERIC ( n : INTEGER := 8 );
  PORT (clk ,rst , en : IN STD_LOGIC;
        c : OUT STD_LOGIC_VECTOR(n DOWNTO 0) 
         );
END counter;

architecture behavioral1 OF counter IS  
SIGNAL counter : STD_LOGIC_VECTOR( n DOWNTO 0);
BEGIN  
 
 process(clk)
 BEGIN 
	if (rising_edge (clk)) then
		if (rst ='1') then
			counter <= (others =>'0');
		elsif (en = '1') then 
			counter <= STD_LOGIC_VECTOR(unsigned(counter)+1); 
		END if;
	END if;
END process;  

c <= counter;

END behavioral1;
------------------------------------------------------------------------------------------------------------
library ieee;
USE ieee.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY single_port_ram_aap IS
  GENERIC ( n : INteger := 15;
			addr_width : INteger := 4 ;
			row : INteger := 4 
			);
  PORT(
    clk: IN STD_LOGIC;
    we : IN STD_LOGIC;
    addr : IN STD_LOGIC_VECTOR(addr_width DOWNTO 0);
    din : IN STD_LOGIC_VECTOR(n DOWNTO 0);
    dout : OUT STD_LOGIC_VECTOR(n DOWNTO 0)
    );
END single_port_ram_aap;

ARCHITECTURE arch OF single_port_ram_aap IS
 TYPE ram_TYPE IS ARRAY (row DOWNTO 0) OF STD_LOGIC_VECTOR (n DOWNTO 0);
 SIGNAL ram_sINgle_PORT : ram_TYPE := (OTHERS => (OTHERS =>'0'));
BEGIN
  PROCESS(clk)
  BEGIN 
    IF (clk'event and clk='1') THEN
      IF (we='1') THEN 
        ram_sINgle_PORT(TO_INTEGER(unsigned(addr))) <= din;
	  ELSE 
  		dout<=ram_sINgle_PORT(TO_INTEGER(unsigned(addr)));
      END IF;
  END IF;
  END PROCESS;


END arch; 
-------------------------------------------------------------------------------------