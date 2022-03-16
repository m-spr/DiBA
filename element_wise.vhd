------- element-wise

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY element_wise IS  
  PORT(
    clk, rst, run: IN STD_LOGIC;
    df: IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	reg_out : OUT STD_LOGIC;
	hout : OUT STD_LOGIC_VECTOR(15 DOWNTO 0)
  );
END element_wise;

ARCHITECTURE behavioral OF element_wise IS 
 

COMPONENT p3 IS  
  PORT(
    h_out : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
    d : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
    pointer_c : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    count_add : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
    clk , count_tanh, count_activation , count_mul,  en_c , reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c: IN STD_LOGIC  
  );
END COMPONENT;

COMPONENT p3_controller IS  
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
    df , 
    pointer_c ,
    count_add ,
    clk , count_tanh, count_activation , count_mul, en_c ,  reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c
  );
  

p_c : p3_controller  
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
TYPE c_mem IS  array (0 TO 127) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
-------------***********************************************************************----------------------
SIGNAL c_ram : c_mem := (others=>(others=>'0'));
SIGNAL sig_out, sig_in , reg_sig,tan_in,tan_in_1, tan_out, reg_tan, mull_in, reg_mull, demux_c, c_reg, demux_add  ,add_out, reg_addo : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL mull_out : STD_LOGIC_VECTOR(62 DOWNTO 0);
SIGNAL c : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL extra :STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL demux_activation : STD_LOGIC_VECTOR(1 DOWNTO 0);
SIGNAL mull_out_reg :STD_LOGIC_VECTOR(31 DOWNTO 0);
BEGIN 
  
  
  
  process (clk)
    BEGIN 
  if (rising_edge(clk)) then
    if (en_c = '1')then
		c_ram(to_INTEGER(unsigned(pointer_c))) <=   demux_c ;
	else
		c <=  c_ram(to_INTEGER(unsigned(pointer_c)));
	END if;
  END if;
  END process;
  
  
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
  
  mull_out(61 DOWNTO 0) <= STD_LOGIC_VECTOR(unsigned(mull_in(30 DOWNTO 0))*unsigned(reg_sig(30 DOWNTO 0)));
  mull_out(62) <= mull_in(31) xor reg_sig(31); 
  mull_out_reg <= mull_out(62)&mull_out(60 DOWNTO 30);
  
  reg_mull_m : reg 
  PORT MAP ( clk , reg_en_mull, mull_out_reg , reg_mull );

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

ENTITY p3_controller IS  
  PORT(clk ,rst, run : IN STD_LOGIC;
	reg_out : OUT STD_LOGIC ; 
    pointer_c_out : OUT STD_LOGIC_VECTOR(9 DOWNTO 0);
    count_add : OUT STD_LOGIC_VECTOR(1 DOWNTO 0);
    count_tanh, count_activation , count_mul : OUT STD_LOGIC ;
    en_c ,reg_en_sig , reg_en_tan , reg_en_mull , reg_en_add_in , reg_en_add_out , reg_en_demux_c : OUT STD_LOGIC
  );
END p3_controller;

ARCHITECTURE ctrl OF p3_controller IS 

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


process(ps)


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
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY demux IS 
  PORT (
        sel : IN STD_LOGIC_VECTOR(1 DOWNTO 0);
        a , b ,c : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
        d : IN STD_LOGIC_VECTOR(31 DOWNTO 0) 
         );
END demux;

architecture behavioral OF demux IS  
SIGNAL sa , sb ,sc ,sd: STD_LOGIC_VECTOR (31 DOWNTO 0);
BEGIN  
 process (d,sel) IS 
BEGIN 
--   sa <= sa;
 --  sb <= sb;
--   sc <= sc;
 if (Sel= "00") then
  a <= d;
 elsif (Sel= "01") then
 b <= d;
 elsif (Sel= "11") then
  c <= d;
 else 
 null;
 END if;
END process;  

END behavioral;
------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY ripple_add_32 IS 
PORT (din1, din2 : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	    dout : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
END ripple_add_32;	  

ARCHITECTURE behavioral OF ripple_add_32 IS 

SIGNAL out1, out2 : STD_LOGIC_VECTOR(30 DOWNTO 0);
SIGNAL en , se11, sel2, sign : STD_LOGIC;
SIGNAL t , d:STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL do : STD_LOGIC_VECTOR(30 DOWNTO 0);


COMPONENT twoscompliment IS 
PORT (en : IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	    dout : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
END COMPONENT;

COMPONENT mux2 IS 
  PORT (
        sel : IN STD_LOGIC;
        a , b : IN STD_LOGIC_VECTOR (30 DOWNTO 0);
        c : OUT STD_LOGIC_VECTOR(30 DOWNTO 0) 
         );
END COMPONENT;

BEGIN 
  
END behavioral;	 
------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.all;

ENTITY tanh IS 
PORT ( 
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      tanhout : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	   );
END tanh;	  

ARCHITECTURE behavioral OF tanh IS 
  SIGNAL a_index :STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL p1_index , p2_index: STD_LOGIC_VECTOR(4 DOWNTO 0);
  SIGNAL din1 , din2 , din3 : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL signed_din : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL h1 , h2 : INTEGER;
BEGIN 
        

process(din)
  BEGIN 
     signed_din(31) <= din (31);
	if (din (31) ='1') then 
		signed_din(30 DOWNTO 0) <= STD_LOGIC_VECTOR(signed(not(din(30 DOWNTO 0)))+1);
	else
		signed_din (30 DOWNTO 0) <= din (30 DOWNTO 0);
	END if; 
END process;

process(signed_din)
BEGIN 
    if (  signed_din(30 DOWNTO 15)  > "0000000000000100"   )then        -- x >4
      p1_index <= "01111" ;
      p2_index <= "01111" ;
      if (signed_din(31) ='1') then
        a_index <=  "11000000000000000000000000000000" ;
      else 
        a_index <= "01000000000000000000000000000000";
      END if;
    elsif ( "11" =  signed_din(16 DOWNTO 15)   )then                       -- x > 3
      p1_index <= "00111" ;
      p2_index <= "11001" ;
      if (signed_din(31) ='1') then
        a_index <=  "10111110101110000101000000000000" ;
      else 
        a_index <= "00111110100001110010110000000000";
      END if;
    
    elsif ( "10" =  signed_din(16 DOWNTO 15)   )then               -- x > 2
      p1_index <= "00101" ;
      p2_index <= "10101" ;
      if (signed_din(31) ='1') then
        a_index <=  "10111010001111010111000000000000" ;
      else 
        a_index <= "00111010001011010001000000000000";
      END if;
    
    elsif ( "11" =  signed_din(15 DOWNTO 14)   )then              -- x > 1.5
      p1_index <= "00011" ;
      p2_index <= "10111" ;
      if (signed_din(31) ='1') then
        a_index <=  "10101111010010111100100000000000" ;
      else 
        a_index <= "00101110101110000101000000000000";  
      END if;
    
    elsif ( "1010101000111101" <  signed_din(15 DOWNTO 0)    )then  -- x > 1.3
      p1_index <= "00010" ;
      p2_index <= "10101" ;
      if (signed_din(31) ='1') then
        a_index <= "10100100111111011111010000000000"  ;
      else 
        a_index <= "00100100111111011111010000000000";
      END if;
    
    elsif ( '1' = signed_din(15)   )then                          -- x > 1
      p1_index <= "00010" ;
      p2_index <= "00100" ;
      if (signed_din(31) ='1') then
        a_index <=  "10011100110011001100110000000000" ;
      else 
        a_index <= "00011101000111101011100000000000";
      END if;
    
    elsif ( "110011001100110"  <  signed_din(14 DOWNTO 0)   )then  -- x>0.8
      p1_index <= "00001" ;
      p2_index <= "10110" ;
      if (signed_din(31) ='1') then
        a_index <= "10010001110010101100000000000000"  ;
      else 
        a_index <="00010001111010111000010000000000" ;
      END if;
    
    elsif ( "101001100110011" <  signed_din(14 DOWNTO 0)    )then -- x> 0.65
      p1_index <= "00001" ;
      p2_index <= "00011" ;
      if (signed_din(31) ='1') then
        a_index <=  "10001010110000001000010000000000" ;
      else 
        a_index <= "00001010101100000010000000000000";
      END if;
    
    elsif ( "010101000111101" <  signed_din(14 DOWNTO 0)    )then -- x < 0.33
      p1_index <= "00001" ;
      p2_index <= "00010" ;
      if (signed_din(31) ='1') then
        a_index <=   "10000101101000011100110000000000";
      else 
        a_index <= "00000101011010000111010000000000";
      END if;
    
    else
      p1_index <= "00001" ;
      p2_index <= "00001" ;
      a_index <=  "00000000000000000000000000000000" ;
      
    END if;
  END process;
  
  h1 <= 15 - to_INTEGER(unsigned (p1_index(3 DOWNTO 0)));
  h2 <= 15 - to_INTEGER(unsigned (p2_index(3 DOWNTO 0)));
  
  din1(30 DOWNTO 0) <=STD_LOGIC_VECTOR( shift_left(unsigned(din(30 DOWNTO 0)),h1));
  din2(30 DOWNTO 0) <=STD_LOGIC_VECTOR( shift_left(unsigned(din(30 DOWNTO 0)),h2));
 
   din1(31) <= p1_index(4) xor din(31);
   din2(31) <= p2_index(4) xor din(31);
  
  
process(din1,din2)
 BEGIN 
  
    if ( din1(31) = din2(31) ) then
       din3 <= din1(31) & STD_LOGIC_VECTOR (unsigned(din1(30 DOWNTO 0)) + unsigned(din2(30 DOWNTO 0)));
    ELSif (din1(30 DOWNTO 0) > din2(30 DOWNTO 0)) then
       din3 <= din1(31) & STD_LOGIC_VECTOR (unsigned(din1(30 DOWNTO 0)) - unsigned(din2(30 DOWNTO 0)));
    else
       din3 <= din2(31) & STD_LOGIC_VECTOR (unsigned(din2(30 DOWNTO 0)) - unsigned(din1(30 DOWNTO 0)));
    END if;
    
 END process;

process(din3,a_index, din, din1)
 BEGIN 
  
    if ( a_index(31) = din3(31) ) then
       tanhout <= din1(31) & STD_LOGIC_VECTOR (unsigned(a_index(30 DOWNTO 0)) + unsigned(din3(30 DOWNTO 0)));
    ELSif (a_index(30 DOWNTO 0) > din3(30 DOWNTO 0)) then
       tanhout <= din1(31) & STD_LOGIC_VECTOR (unsigned(a_index(30 DOWNTO 0)) - unsigned(din3(30 DOWNTO 0)));
    else
       tanhout <= din3(31) & STD_LOGIC_VECTOR (unsigned(din3(30 DOWNTO 0)) - unsigned(a_index(30 DOWNTO 0)));
    END if;
    
 END process;


END behavioral;	 
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
      sigout : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	   );
END sigmoid;	  

ARCHITECTURE behavioral OF sigmoid IS 
  SIGNAL a_index :STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL p1_index , p2_index: STD_LOGIC_VECTOR(4 DOWNTO 0);
  SIGNAL din1 , din2 , din3 : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL signed_din : STD_LOGIC_VECTOR(31 DOWNTO 0);
  SIGNAL h1 , h2 : INTEGER;
BEGIN 
        

process(din)
  BEGIN 
     signed_din(31) <= din (31);
	if (din (31) ='1') then 
		signed_din(30 DOWNTO 0) <= STD_LOGIC_VECTOR(signed(not(din(30 DOWNTO 0)))+1);
	else
		signed_din (30 DOWNTO 0) <= din (30 DOWNTO 0);
	END if; 
END process;
process(signed_din)
BEGIN 
		
    if ( "000000000000110" <  signed_din(30 DOWNTO 15)   )then -- x > 6
      p1_index <= "00000" ;
      p2_index <= "00000" ;
      if (signed_din(31) ='0') then
        a_index <=   "01000000000000000000000000000000";
      else 
        a_index <= "00000000000000000000000000000000";
      END if;
    elsif ( "101" =  signed_din(17 DOWNTO 15)   )then -- x > 5
      p1_index <= "01000" ;
      p2_index <= "00000" ;
      if (signed_din(31) ='0') then
        a_index <=  "00111110010101100000010000000000" ;
      else 
        a_index <= "00000001100110011001100000000000";
      END if;
    elsif ( "100" =  signed_din(17 DOWNTO 15)   )then -- x > 4
      p1_index <= "00110" ;
      p2_index <= "00000" ;
      if (signed_din(31) ='0') then
        a_index <= "00111110010101100000010000000000" ;
      else 
        a_index <= "00000101010011111110000000000000" ;
	   END if;
    elsif ( "11" =  signed_din(16 DOWNTO 15)   )then -- x > 3
      p1_index <= "00101" ;
      p2_index <= "11001" ;
      if (signed_din(31) ='0') then
        a_index <=  "00110111010111000010100000000000" ;
      else 
        a_index <= "00001000101000111101100000000000" ;
	  END if;
    elsif ( "10" =  signed_din(16 DOWNTO 15)   )then -- x > 2
      p1_index <= "00100" ;
      p2_index <= "00111" ;
      if (signed_din(31) ='0') then
        a_index <=  "00101111101011100001010000000000" ;
      else 
        a_index <= "00010000011000100100110000000000" ;
      END if;
    elsif ( "11" =  signed_din(15 DOWNTO 14)   )then  -- x > 1.5
      p1_index <= "00011" ;
      p2_index <= "11000" ;    
      if (signed_din(31) ='0') then
        a_index <= "00101000111001010110000000000000" ;
      else 
        a_index <= "00010111001010110000010000000000" ;
      END if;
    elsif ( "1001100110011001" <  signed_din(15 DOWNTO 0)    )then  -- x > 1.2
      p1_index <= "00011" ;
      p2_index <= "00101" ;
      if (signed_din(31) ='0') then
        a_index <=  "00100101010011111110000000000000" ;
      else 
        a_index <=  "00011010110000001000010000000000" ;
      END if;
    elsif ( '1' = signed_din(15)   )then -- x > 1
      p1_index <= "00011" ;
      p2_index <= "00100" ;
      if (signed_din(31) ='0') then
        a_index <=  "00100010111000010100100000000000";
      else 
        a_index <= "00011101001111110111110000000000" ;
      END if;
    elsif ( "110011001100110"  <  signed_din(14 DOWNTO 0)   )then  -- x>0.8
      p1_index <= "00010" ;
      p2_index <= "10101" ;
      if (signed_din(31) ='0') then
        a_index <=  "00100000111001010110000000000000" ;
      else 
        a_index <=  "00011111001010110000010000000000" ;
      END if;
    else
      p1_index <= "00010" ;
      p2_index <= "10110" ;
      a_index <=  "00100000000000000000000000000000" ;
    END if;
  END process;
  
  
  h1 <= 15 - to_INTEGER(unsigned (p1_index(3 DOWNTO 0)));
  h2 <= 15 - to_INTEGER(unsigned (p2_index(3 DOWNTO 0)));
  din1(30 DOWNTO 0) <=STD_LOGIC_VECTOR( shift_left(unsigned(din(30 DOWNTO 0)),h1));
  din2(30 DOWNTO 0) <=STD_LOGIC_VECTOR( shift_left(unsigned(din(30 DOWNTO 0)),h2));
  din1(31) <= p1_index(4) xor din(31);
  din2(31) <= p2_index(4) xor din(31);
process(din1,din2)
 BEGIN 
  
    if ( din1(31) = din2(31) ) then
       din3 <= din1(31) & STD_LOGIC_VECTOR (unsigned(din1(30 DOWNTO 0)) + unsigned(din2(30 DOWNTO 0)));
    ELSif (din1(30 DOWNTO 0) > din2(30 DOWNTO 0)) then
       din3 <= din1(31) & STD_LOGIC_VECTOR (unsigned(din1(30 DOWNTO 0)) - unsigned(din2(30 DOWNTO 0)));
    else
       din3 <= din2(31) & STD_LOGIC_VECTOR (unsigned(din2(30 DOWNTO 0)) - unsigned(din1(30 DOWNTO 0)));
    END if;
    
 END process;

process(din3,a_index, din1)
 BEGIN 
  
    if ( a_index(31) = din3(31) ) then
       sigout <= din1(31) & STD_LOGIC_VECTOR (unsigned(a_index(30 DOWNTO 0)) + unsigned(din3(30 DOWNTO 0)));
    ELSif (a_index(30 DOWNTO 0) > din3(30 DOWNTO 0)) then
       sigout <= din1(31) & STD_LOGIC_VECTOR (unsigned(a_index(30 DOWNTO 0)) - unsigned(din3(30 DOWNTO 0)));
    else
       sigout <= din3(31) & STD_LOGIC_VECTOR (unsigned(din3(30 DOWNTO 0)) - unsigned(a_index(30 DOWNTO 0)));
    END if;
    
 END process;

END behavioral;

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