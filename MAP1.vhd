------ ****  matrix_multiplier_plate1   ***** --------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL; 


ENTITY MAP_1 IS 
GENERIC(
      n : INTEGER := 15
        );
PORT ( 
      clk,  rst ,add_reg , reg_out: IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
      w : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
      dout : OUT STD_LOGIC_VECTOR (31 DOWNTO 0)
	   );
END MAP_1;	  


ARCHITECTURE behavioral_MAP1 OF MAP_1 IS 

	COMPONENT MAP_add IS 
	  PORT ( 
		  clk,  rst,  add_reg,  reg_out: IN STD_LOGIC;
		  din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
		  dout : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
	END COMPONENT;

SIGNAL mul_OUT , mul : STD_LOGIC_VECTOR(31 DOWNTO 0);


BEGIN 

add : MAP_add 
  PORT MAP( clk,  rst,  add_reg,  reg_out,  mul_out, dout );
		
    
mul_OUT <= STD_LOGIC_VECTOR ( signed(din) * signed(w));

--mul<= STD_LOGIC_VECTOR( shIFt_right(signed(mul_out),12));


END behavioral_MAP1;	 

-----------------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY MAP_add IS 
PORT ( 
      clk,  rst, add_reg, reg_out: IN STD_LOGIC;
      din : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	  dout : OUT STD_LOGIC_VECTOR(31 DOWNTO 0));
END MAP_add;	  

ARCHITECTURE behaviorALL OF MAP_add IS 

COMPONENT adder32CLA4 IS
	PORT ( 
		a, b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
		cIN : IN STD_LOGIC;
		sum : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		cOUT : OUT STD_LOGIC;
		G, P : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	);
END Component;

SIGNAL val1,  d_reg1,  d_reg2 : STD_LOGIC_VECTOR(31 DOWNTO 0);
SIGNAL cOUT : STD_LOGIC;
SIGNAL G, P : STD_LOGIC_VECTOR(7 DOWNTO 0);
BEGIN  
	 PROCESS(clk)
		 BEGIN 
		   IF rISINg_edge(clk )THEN
			  IF ( rst ='1') THEN 
				  d_reg1 <= (OTHERS=>'0');
				  d_reg2 <= (OTHERS=>'0');
			  ELSIF(add_reg = '1')THEN
				IF (reg_OUT = '1') THEN
				   d_reg2 <= d_reg1;
				   d_reg1 <= din;
				ELSE 
					d_reg1 <= val1;
					d_reg2 <= d_reg2;
				END iF;
			  ELSIF (reg_OUT ='1') THEN
				   d_reg1 <= (OTHERS=>'0');
				   d_reg2 <= d_reg1;
			  ELSE
					d_reg1 <= d_reg1;
					d_reg2 <= d_reg2;
			  END IF;

		 END IF;
	END PROCESS; 

	add_cla : adder32CLA4 
		PORT MAP( 
			din, d_reg1 ,'0',val1,
			cOUT ,
			G, P 
		);

	dout <= d_reg2 ;   


END behaviorALL;	
-----------------------------------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY MAP1_controller IS
GENERIC(
		n : INTEGER := 16
		); -- = n
PORT ( 
	clk, rst, run, paUSE : IN STD_LOGIC;
	add_reg, out_reg :OUT STD_LOGIC 
  );       
END MAP1_controller;	  

ARCHITECTURE cnt OF MAP1_controller Is

COMPONENT counter IS
	GENERIC ( n : INTEGER := 8);
	PORT ( 
		  clk ,rst , en : IN STD_LOGIC;
          c : OUT STD_LOGIC_VECTOR(n DOWNTO 0) );
END COMPONENT;

  SIGNAL add_row_i , r_i , count: STD_LOGIC_VECTOR (4 DOWNTO 0);
  TYPE mINi_state IS  (INit, paUSE_END_reg_state, paUSE_mull_state,mull,mull_END); -- add_paUSE,  
  SIGNAL ns,  ps : mINi_state;
  SIGNAL count_r , rst_count_r : STD_LOGIC;
  
BEGIN 
-- c <= STD_LOGIC_VECTOR(TO_UNSIGNED(n , 4));
PROCESS(clk) BEGIN 
  IF rISINg_edge(clk) THEN
    IF (rst ='1')THEN
      ps <= INit; 
    ELSE  
      ps <= ns;  
    END IF;
  END IF;
END PROCESS;

PROCESS ( ps,  run, paUSE, add_row_i )
  BEGIN 
    add_reg <= '0';
    out_reg <= '0';
	count_r <= '0';
	rst_count_r <= '0';
  CASE (ps) IS 
    WHEN INit => 
		rst_count_r <= '1';
	IF (run ='1') THEN 
			ns <= mull;
			add_reg <= '1';
			count_r <= '1';
			rst_count_r <= '0';
		ELSE 
			ns <= INit;
		END IF;
	WHEN mull =>
		add_reg <= '1';
		count_r <= '1';
		IF ( add_row_i = r_i) THEN 
			ns <= mull_END;
			rst_count_r <= '1';
		ELSIF ( paUSE = '1') THEN 
			ns <= paUSE_mull_state;
		ELSE 
			ns <= mull;
		END IF;
	WHEN paUSE_mull_state =>
		IF ( paUSE = '1') THEN 
			ns <= paUSE_mull_state;
		ELSIF ( add_row_i = r_i) THEN 
			ns <= mull_END;
			rst_count_r <= '1';
		ELSE 
			ns <= mull;
		END IF;		
    WHEN mull_END =>
		add_reg <= '1';
		out_reg <= '1';
		count_r <= '1';
		IF ( paUSE = '1') THEN 
			ns <= paUSE_END_reg_state;
		ELSE 
			ns <= mull;
		END IF;
    WHEN paUSE_END_reg_state =>
		IF ( paUSE = '1') THEN 
			ns <= paUSE_END_reg_state;
		ELSE
			ns <= mull;
		END IF;		
    WHEN OTHERS =>
			ns <= INit;
   END CASE;
END PROCESS;
 
	count_s : counter 
		GENERIC MAP(4)
		PORT MAP( 
			  clk, rst_count_r, count_r , add_row_i);
		  
	r_i <= STD_LOGIC_VECTOR(TO_UNSIGNED((n -1), 5));

	count <= add_row_i;
		  
END cnt;	 
------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;


ENTITY counter IS 
  GENERIC ( n : INTEGER := 8 );
  PORT (clk ,rst , en : IN STD_LOGIC;
        c : OUT STD_LOGIC_VECTOR(n DOWNTO 0) 
         );
END counter;

ARCHITECTURE behavioral1 OF counter IS  
SIGNAL counter : STD_LOGIC_VECTOR( n DOWNTO 0);
BEGIN  
 
 PROCESS(clk)
 BEGIN 
	IF (rISINg_edge (clk)) THEN
		IF (rst ='1') THEN
			counter <= (OTHERS =>'0');
		elsIF (en = '1') THEN 
			counter <= STD_LOGIC_VECTOR(unsigned(counter)+1); 
		END IF;
	END IF;
END PROCESS;  

c <= counter;

END behavioral1;
--------------------------------------------------------------
LIBRARY ieee;
USE ieee.STD_LOGIC_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY MAP1_controller_tb IS

END MAP1_controller_tb;	  

ARCHITECTURE tb OF MAP1_controller_tb Is

Component MAP1_controller IS
GENERIC(
		n : INTEGER := 16
		); -- = n
PORT ( 
	clk, rst, run, paUSE : IN STD_LOGIC;
	add_reg, out_reg :OUT STD_LOGIC   );       
END COMPONENT;

SIGNAL clk, rst, run, paUSE :  STD_LOGIC := '1';
SIGNAL add_reg, out_reg : STD_LOGIC ;

BEGIN 

MAP1_cnt : MAP1_controller
GENERIC MAP(
		8
		) 
PORT MAP( 
	clk, rst, run, paUSE ,
	add_reg, out_reg  
  );       
clk <= not clk AFTER 5 ns;
rst <= '0' AFTER 30 ns;
run <= '0',  '1' AFTER 50 ns;
paUSE <= '0', '1' AFTER 180 ns , '0' AFTER 200 ns;
END tb;