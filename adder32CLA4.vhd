--
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
ENTITY adder32CLA4 IS
	PORT ( 
		a, b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
		cIN : IN STD_LOGIC;
		sum : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
		cOUT : OUT STD_LOGIC;
		G, P : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	);
END adder32CLA4;	  

ARCHITECTURE behavioral OF adder32CLA4 IS
	COMPONENT CLA4bit IS
		PORT ( 
			a, b : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
			cIN : IN STD_LOGIC;
			s : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
			co : OUT STD_LOGIC;
			Ggroup, Pgroup : OUT STD_LOGIC
		);
	END COMPONENT;

	SIGNAL CY : STD_LOGIC_VECTOR(8 DOWNTO 0);
BEGIN
	INST : FOR I IN 0 TO 7 GENERATE
		cla4 : CLA4bit  	
			PORT MAP( a => a(4*i+3 DOWNTO 4*i), b => b(4*i+3 DOWNTO 4*i), 
					   cIN => CY(i), s => sum(4*i+3 DOWNTO 4*i), 
					   co => CY(i+1), Ggroup => G(i), Pgroup => P(i)
			);
	END GENERATE;

	CY(0) <= cIN;
	cOUT <= CY(8);	
END behavioral;	
---------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
ENTITY adder32CLA4_TB IS
END adder32CLA4_TB;	  

ARCHITECTURE behavioral OF adder32CLA4_TB IS
	COMPONENT  adder32CLA4 IS
		PORT ( 
			a, b : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
			cIN : IN STD_LOGIC;
			sum : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
			cOUT : OUT STD_LOGIC;
			G, P : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
		);
	END COMPONENT;

	SIGNAL a, b, sum : STD_LOGIC_VECTOR(31 DOWNTO 0);
	SIGNAL Ggroup, Pgroup : STD_LOGIC_VECTOR(7 DOWNTO 0);
	SIGNAL cIN, co : STD_LOGIC; 
BEGIN
	adder32CLA4_test : adder32CLA4  	
		PORT MAP(
			a, b, cIN, sum, co, Ggroup, Pgroup
		);
		
	a <= "00000001010110101010101010001000", "10110101011010101010101000100001" AFTER 2 NS, "00000010110101010101010001000001" AFTER 5 NS; 
	b <= "00000001101010101010100010001110", "00000000010110101010101010001000" AFTER 2 NS, "10110101011010101010101000100011" AFTER 3 NS; 
	cIN <= '0', '1' AFTER 1 NS, '0' AFTER 4 NS; 
END behavioral;
---------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
ENTITY FA IS
	PORT ( 
		a, b, cIN : IN STD_LOGIC;
		s, co : OUT STD_LOGIC
	);
END FA;	  

ARCHITECTURE behavioral OF FA IS
BEGIN
	s <= a XOR b XOR cIN;
	co <= (a AND b) OR (a AND cIN) OR (b AND cIN);
END behavioral;	
---------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
ENTITY CLG4 IS
	PORT ( 
		g, p : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
		cIN : IN STD_LOGIC;
		carry : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
		Ggroup, Pgroup : OUT STD_LOGIC
	);
END CLG4;	  

ARCHITECTURE behavioral OF CLG4 IS
BEGIN
	carry(0) <= cIN;
	carry (4) <= ((g(3)) OR (g(2) AND p(3)) OR
				(g(1) AND p(2) AND p(3)) OR
				(g(0) AND p(1) AND p(2) AND p(3)) OR
				(p(0) AND p(1) AND p(2) AND p(3) AND cIN)); 
	carry (3) <= ((g(2)) OR
				(g(1) AND p(2)) OR
				(g(0) AND p(1) AND p(2)) OR
				(p(0) AND p(1) AND p(2) AND cIN));
	carry (2) <= ((g(1)) OR
				(g(0) AND p(1)) OR
				(p(0) AND p(1) AND cIN));
	carry (1) <= (g(0) OR (p(0) AND cIN)) ;

	Ggroup <= (g(3) OR (p(3) AND g(2)) OR (p(3) AND p(2) AND g(1)) OR (p(3) AND p(2) AND p(1) AND g(0))) ;
	Pgroup <= (p(3) AND p(2) AND p(1) AND p(0)); 
END behavioral;	
---------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
ENTITY GPgen4 IS
	PORT ( 
		a, b : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
		g, p : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
	);
END GPgen4;	  

ARCHITECTURE behavioral OF GPgen4 IS
BEGIN
	g <= a AND b; 
	p <= a XOR b; 
END behavioral;	
---------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
ENTITY CLA4bit IS
	PORT ( 
		a, b : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
		cIN : IN STD_LOGIC;
		s : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
		co : OUT STD_LOGIC;
		Ggroup, Pgroup : OUT STD_LOGIC
		);
END CLA4bit;	  

ARCHITECTURE behavioral OF CLA4bit IS
	COMPONENT  GPgen4 IS
		PORT ( 
			a, b : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
			g, p : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
		);
	END COMPONENT;
	
	COMPONENT  CLG4 IS
		PORT ( 
			g, p : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
			cIN : IN STD_LOGIC;
			carry : OUT STD_LOGIC_VECTOR(4 DOWNTO 0);
			Ggroup, Pgroup : OUT STD_LOGIC
		);
	END COMPONENT;

	SIGNAL p, g : STD_LOGIC_VECTOR(3 DOWNTO 0);
	SIGNAL cry : STD_LOGIC_VECTOR(4 DOWNTO 0); 
BEGIN
	gap4 : GPgen4  	
		PORT MAP(
			a => a, b => b, g => g, p => p
		);
		
	clggen : CLG4  	
		PORT MAP(
			p => p, g => g, cIN => cIN, carry => cry, Ggroup => Ggroup, Pgroup => Pgroup
		);
		
	co <= cry(4); 
	s(3) <= p(3) XOR cry(3);
	s(2) <= p(2) XOR cry(2);
	s(1) <= p(1) XOR cry(1);
	s(0) <= p(0) XOR cry(0);
END behavioral;	
---------------------------------------------------------------------------------------------------
LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.numeric_std.ALL;
ENTITY CLA4bit_TB IS
END CLA4bit_TB;	  

ARCHITECTURE behavioral OF CLA4bit_TB IS
	COMPONENT  CLA4bit IS
		PORT ( 
			a, b : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
			cIN : IN STD_LOGIC;
			s : OUT STD_LOGIC_VECTOR(3 DOWNTO 0);
			co : OUT STD_LOGIC;
			Ggroup, Pgroup : OUT STD_LOGIC
		);
	END COMPONENT;

	SIGNAL a, b, s : STD_LOGIC_VECTOR(3 DOWNTO 0);
	SIGNAL cIN, co, Ggroup, Pgroup : STD_LOGIC; 
BEGIN
	CLA4bit_test : CLA4bit  	
		PORT MAP(
			a, b, cIN, s, co, Ggroup, Pgroup
		);
		
	a <= "0110", "0101" AFTER 2 NS, "1001" AFTER 5 NS; 
	b <= "1110", "0111" AFTER 2 NS, "1011" AFTER 3 NS; 
	cIN <= '0', '1' AFTER 1 NS, '0' AFTER 4 NS; 
END behavioral;


