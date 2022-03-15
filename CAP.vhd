---- convergence addINg plate

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY CAP IS 
  GENERIC(
      n : INTEGER := 16
        );
  PORT ( clk, rst, run : IN STD_LOGIC ;
        reg_OUT : OUT STD_LOGIC;
	    dIN : IN STD_LOGIC_VECTOR(32*n-1 DOWNTO 0);
        dout: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
       );
END CAP;	  


ARCHITECTURE behavioral_CAP OF CAP IS 

	COMPONENT  mux_cascadINg IS 
	  GENERIC ( n : INTEGER := 8);
	  PORT (
			sel : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
			dIN : IN STD_LOGIC_VECTOR (32*n-1 DOWNTO 0);
			dOUT : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
			);
	END COMPONENT;


	COMPONENT CAP_controller IS 
	   GENERIC ( n : INTEGER := 8);
		PORT ( 
		  clk, rst, run : IN STD_LOGIC;
		  sel : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
		  IN_reg, add_reg, out_reg : OUT STD_LOGIC );  
	END COMPONENT;


	COMPONENT add IS 
	  PORT (
			clk , IN_reg , add_reg , reg_OUT : IN STD_LOGIC;
			dIN : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
			dOUT : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
			);
	END COMPONENT;

SIGNAL doutm : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL add_reg,IN_reg, out_reg: STD_LOGIC;
SIGNAL sel : STD_LOGIC_VECTOR(3 DOWNTO 0);


BEGIN 


	  C_add : add
		   PORT MAP(clk ,IN_reg,add_reg ,out_reg ,
			doutm,
			dout
			);
	  mux : mux_cascadINg 
		  GENERIC MAP(n)
		  PORT MAP(
				sel ,
				dIN,
				doutm
				);      
	  c_add_ctrl : CAP_controller 
		GENERIC MAP( n )
		PORT MAP(clk, rst, run ,
			  sel ,
			  IN_reg, add_reg, out_reg  );
		 
		 
     reg_OUT <= out_reg;
END behavioral_CAP;
------------------------------------------------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;   
USE IEEE.NUMERIC_STD.ALL;

ENTITY CAP_controller IS 
	GENERIC ( n : INTEGER := 8);
	PORT ( 
		  clk, rst, run : IN STD_LOGIC;
		  sel : OUT STD_LOGIC_VECTOR (3 DOWNTO 0);
		  IN_reg, add_reg, out_reg : OUT STD_LOGIC );       
END CAP_controller;	  

ARCHITECTURE contr OF CAP_controller IS 

COMPONENT counter IS
	GENERIC ( n : INTEGER := 8 );
  PORT (clk ,rst , en : IN STD_LOGIC;
        c : OUT STD_LOGIC_VECTOR(n DOWNTO 0) 
         );
END COMPONENT;

  TYPE CAP_states IS  (INit,  add_state,  END_reg_state); -- add_paUSE,  
  SIGNAL ns,  ps : CAP_states;
  SIGNAL count_r , rst_count_r : STD_LOGIC;
  SIGNAL add_row_i : STD_LOGIC_VECTOR(3 DOWNTO 0);
  
BEGIN 
-- c <= STD_LOGIC_VECTOR(to_UNSIGNED(n , 4));
PROCESS(clk) BEGIN 
  IF rISINg_edge(clk) THEN
    IF (rst ='1')THEN
      ps <= INit; 
    ELSE  
      ps <= ns;  
    END IF;
  END IF;
END PROCESS;

PROCESS ( ps,  run,add_row_i )
  BEGIN 
	add_reg <= '0';
    out_reg <= '0';
	count_r <= '0';
	IN_reg <= '0';
	rst_count_r <= '0';
	  CASE (ps) IS 
		WHEN INit =>
				rst_count_r <= '1';
				IN_reg <= '1';
			IF ( run = '1') THEN
				ns <= add_state;
			ELSE
				ns <= INit;
			END IF;
		WHEN add_state =>
			count_r <= '1';
			add_reg <= '1';
			IF ( add_row_i = STD_LOGIC_VECTOR(to_UNSIGNED(n-1 , 4))) THEN 
					ns <= END_reg_state;
					rst_count_r <= '1';						
			ELSE 
				ns <= add_state;
			END IF;
		WHEN END_reg_state =>
			add_reg <= '1';
			out_reg <= '1';
			count_r <= '1';
			IF ( run = '1') THEN
				ns <= add_state;
			ELSE
				ns <= INit;
			END IF;
		WHEN OTHERS =>
				ns <= INit;
	   END CASE;
END PROCESS;
 
r_count : counter 
	GENERIC MAP(3)
	PORT MAP( 
		  clk, rst_count_r,  count_r , add_row_i);
	
	sel <= add_row_i;
		 
END contr;


------------------------------------------------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL; 

ENTITY mux_cascadINg IS 
  GENERIC ( n : INTEGER := 8);
  PORT (
        sel : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
        dIN : IN STD_LOGIC_VECTOR (32*n-1 DOWNTO 0);
        dOUT : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
END mux_cascadINg;

ARCHITECTURE behavioral OF mux_cascadINg IS  

SIGNAL data_IN_MUX : STD_LOGIC_VECTOR (32*16-1 DOWNTO 0);
BEGIN  
	 PROCESS ( dIN)
	 BEGIN  
		IF( n < 16) THEN
			data_IN_MUX(32*n-1 DOWNTO 0)  <= dIN;
			data_IN_MUX(511 DOWNTO 32*n)  <= (OTHERS => '0');
		ELSE
			data_IN_MUX <= dIN;
		END IF;
	 END PROCESS;
	 PROCESS(sel , data_IN_MUX)
		 BEGIN 
		  CASE (sel)  IS  
			WHEN "0000" => dOUT <= data_IN_MUX(31 DOWNTO 0) ;
			WHEN "0001" => dOUT <= data_IN_MUX(63 DOWNTO 32) ;
			WHEN "0010" => dOUT <= data_IN_MUX(95 DOWNTO 64) ;
			WHEN "0011" => dOUT <= data_IN_MUX(127 DOWNTO 96) ;
			WHEN "0100" => dOUT <= data_IN_MUX(159 DOWNTO 128) ;
			WHEN "0101" => dOUT <= data_IN_MUX(191 DOWNTO 160) ;
			WHEN "0110" => dOUT <= data_IN_MUX(223 DOWNTO 192) ;
			WHEN "0111" => dOUT <= data_IN_MUX(255 DOWNTO 224) ;
		    WHEN "1000" => dOUT <= data_IN_MUX(287 DOWNTO 256) ;
		    WHEN "1001" => dOUT <= data_IN_MUX(319 DOWNTO 288) ;
		    WHEN "1010" => dOUT <= data_IN_MUX(351 DOWNTO 320) ;
		    WHEN "1011" => dOUT <= data_IN_MUX(383 DOWNTO 352) ;
		    WHEN "1100" => dOUT <= data_IN_MUX(415 DOWNTO 384) ;
		    WHEN "1101" => dOUT <= data_IN_MUX(447 DOWNTO 416) ;
		    WHEN "1110" => dOUT <= data_IN_MUX(479 DOWNTO 448) ;
		    WHEN "1111" => dOUT <= data_IN_MUX(511 DOWNTO 480) ;
			WHEN OTHERS => dOUT <= (OTHERS =>'0'); 
		  END CASE;
		  
	 END PROCESS;  

END behavioral;	 

------------------------------------------------------------------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL; 

ENTITY add IS 
  PORT (clk , IN_reg , add_reg , reg_OUT : IN STD_LOGIC;
        dIN : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
        dOUT : OUT STD_LOGIC_VECTOR(31 DOWNTO 0)
        );
END add;

ARCHITECTURE behavioral OF add IS  

SIGNAL  val1, outm1, d_reg1 :STD_LOGIC_VECTOR(31 DOWNTO 0);


BEGIN  
	 PROCESS(clk)
		 BEGIN 
		   IF rISINg_edge(clk )THEN
			  IF ( IN_reg ='1') THEN 
				  d_reg1 <= (OTHERS=>'0');
			  ELSIF(add_reg = '1')THEN
				IF (reg_OUT ='1') THEN
				   outm1 <= d_reg1;
				   d_reg1 <= dIN;
				ELSE 
					d_reg1 <= val1;
				END iF;
			  ELSIF (reg_OUT ='1') THEN
				   d_reg1 <= (OTHERS=>'0');
				   outm1 <= d_reg1;
			  ELSE
					d_reg1 <= d_reg1;
					outm1 <= outm1;
			  END IF;

		 END IF;
	END PROCESS; 

	val1 <= STD_LOGIC_VECTOR (SIGNED(dIN) + SIGNED(d_reg1));

	dOUT <= outm1 ;   


END behavioral;	 

------------------------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY CAP_tb IS 

END CAP_tb;	  


ARCHITECTURE tb OF CAP_tb IS 

COMPONENT CAP 
  GENERIC(
      n : INTEGER := 16
        );
  PORT ( clk, rst, run : IN STD_LOGIC ;
        reg_OUT : OUT STD_LOGIC;
	    dIN : IN STD_LOGIC_VECTOR(32*n-1 DOWNTO 0);
        dout: OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
       );
END COMPONENT;

SIGNAL clk, rst, run :  STD_LOGIC := '1';
SIGNAL reg_OUT :  STD_LOGIC;
SIGNAL dIN :  STD_LOGIC_VECTOR(32*5-1 DOWNTO 0) := "0000000000000000000000000000000100000000000000000000000000000010000000000000000000000000000001000000000000000000000000000000100000000000000000000000000000010000";
SIGNAL dout:  STD_LOGIC_VECTOR (31 DOWNTO 0);

BEGIN
CAP_1 : CAP 
		GENERIC MAP (5)
		PORT MAP( 
			clk,  rst,  run,  
			reg_OUT ,  
			dIN,  
			dout
				);
		
clk <= not clk AFTER 10 ns;
run <= not run AFTER 70 ns; 
rst <= '0' AFTER 30 ns;
END tb;

----------------------------

	
	