
-- MIPS PIPELINE - VERSAO COM TRATAMENTO DE CONFLITOS DE DADOS 
-- 					 E TRATAMENTO DE CONFLITO DE CONTROLE POR PREDICAO DINAMICA
--                 DIEGO HENRIQUE S. OLIVEIRA, ICARO STUMPF, LEONARDO BARBOSA DA ROSA
--						 NOVEMBRO/2020


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- package com os tipos basicos auxiliares para descrever o processador
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;

package p_MIPS_Pipeline is  
    
    -- inst_type define as instrucoes decodificaveis pelo bloco de controle
    type inst_type is  
            ( ADDU, SUBU, AAND, OOR, XXOR, NNOR, SSLL, SLLV, SSRA, SRAV,
				SSRL, SRLV,ADDIU, ANDI, ORI, XORI, LUI, LBU, LW, SB, SW, SLT,
				SLTU, SLTI,	SLTIU, BEQ, BGEZ, BLEZ, BNE, J, JAL, JALR, JR, 
				NOP, invalid_instruction);
 
    type microinstruction is record
				inst_branch: std_logic; -- identify branch instructions
				inst_grupo1: std_logic; -- identify R instructions
				inst_grupoI: std_logic; -- identify immediate data instructions
            wreg:  std_logic;       -- register bank write enable
            ce:    std_logic;       -- Chip enable and R_W controls
            rw:    std_logic;
            bw:    std_logic;       -- Byte-word control (mem write only)
            i:     inst_type;       -- operation specification
    end record;

	 type DataConflict is record
				opA : std_logic_vector(1 downto 0);
				opB : std_logic_vector(1 downto 0);
				bubble: std_logic;
    end record;
	 
	 type DynamicPrediction is record
				VB : std_logic; -- validation bit
				FSM : std_logic_vector(1 downto 0); -- Finite State Machine
				address : std_logic_vector(31 downto 0); -- branch instruction address
				b_address : std_logic_vector(31 downto 0); -- branch result address
	 end record;
	 

--++++++++++++++
-- Pipeline Registers
--++++++++++++++

	type BIDI is record
			npc : std_logic_vector(31 downto 0); -- pc + 4
			IR : std_logic_vector(31 downto 0); -- instruction code
			pc : std_logic_vector(31 downto 0); -- pc
	end record;
	
	type DIEX is record
			npc: std_logic_vector(31 downto 0); -- pc + 4
			pc : std_logic_vector(31 downto 0); -- pc
			RA : std_logic_vector(31 downto 0); -- Rs loaded value	
			RB : std_logic_vector(31 downto 0); -- Rt loaded value	
			IMED : std_logic_vector(31 downto 0); -- Immediate value
			RS : std_logic_vector(4 downto 0); -- Rs address
			RD : std_logic_vector(4 downto 0); -- Rd address
			RT : std_logic_vector(4 downto 0); -- Rt address
			uins : microinstruction;
		end record;
		
	 type EXMEM is record
			npc : std_logic_vector(31 downto 0); -- pc + 4
			salta : std_logic;
			RALU : std_logic_vector(31 downto 0); -- ULA's output
			adD : std_logic_vector(4 downto 0); -- Write Bank Address
			RB : std_logic_vector(31 downto 0); -- Rt loaded value (for sw instructions)
			uins : microinstruction;
		end record;
		
		type MEMER is record
			npc : std_logic_vector(31 downto 0); -- pc + 4
			adD : std_logic_vector(4 downto 0); -- Write Bank Address
			RALU : std_logic_vector(31 downto 0); -- ULA's output
			MDR : std_logic_vector(31 downto 0); -- read memory value
			uins : microinstruction;
		end record;
         
end p_MIPS_Pipeline;


--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Registrador de uso geral - sensivel a borda de subida do relogio (ck), 
--		com reset assincrono (rst) e habilitacao de escrita (ce)
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;

entity regnbits is
           generic( INIT_VALUE : STD_LOGIC_VECTOR(31 downto 0) := (others=>'0'); 
								EDGE: STD_LOGIC := '0');
           port(  ck, rst, ce : in std_logic;
                  D : in  STD_LOGIC_VECTOR (31 downto 0);
                  Q : out STD_LOGIC_VECTOR (31 downto 0)
               );
end regnbits;

architecture regnbits of regnbits is 
begin

  process(ck, rst)
  begin
       if rst = '1' then
              Q <= INIT_VALUE(31 downto 0);
       elsif ck'event and ck = '1' then
           if ce = '1' then
              Q <= D; 
           end if;
       end if;
  end process;
        
end regnbits;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Banco de Registradores  (R0..R31) - 31 registradores de 32 bits
-- 	Trata-se de uma memoria com tres portas de acesso, nao confundir
--		com a memoria principal do processador. 
--		So duas portas de leitura (sinais AdRP1+DataRP1 e AdRP2+DataRP2) e
--		uma porta de escrita (definida pelo conjunto de sinais ck, rst,
--		ce, AdWP e DataWP).
--		Os endereos de cada porta (AdRP1, AdRP2 e AdWP) sao obviamente de
--		5 bits (pois 2^5=32), enquanto que os barramentos de dados de 
--		sada (DataRP1, DataRP2) e de entrada (DataWP) sao de 32 bits.
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use ieee.STD_LOGIC_UNSIGNED.all;   
use work.p_MIPS_Pipeline.all;

entity reg_bank is
       port( ck, rst, ce :    in std_logic;
             AdRP1, AdRP2, AdWP : in std_logic_vector( 4 downto 0);
             DataWP : in std_logic_vector(31 downto 0);
             DataRP1, DataRP2: out std_logic_vector(31 downto 0) 
           );
end reg_bank;

architecture reg_bank of reg_bank is
   type wirebank is array(0 to 31) of std_logic_vector(31 downto 0);
   signal reg : wirebank ;                            
   signal wen : std_logic_vector(31 downto 0) ;
begin            

    g1: for i in 0 to 31 generate        

        -- Remember register $0 is the constant 0, not a register.
        -- This is implemented by never enabling writes to register $0
        wen(i) <= '1' when i/=0 and AdWP=i and ce='1' else '0';
         
        -- Remember register $29, the stack pointer, points to some place
        -- near the bottom of the data memory, not the usual place 
		-- assigned by the MIPS simulator!!
        g2: if i=29 generate -- SP ---  x10010000 + x800 -- top of stack
           r29: entity work.regnbits generic map(INIT_VALUE=>x"10010800", EDGE=>'1')    
                port map(ck=>ck, rst=>rst, ce=>wen(i), D=>DataWP, Q=>reg(i));
        end generate;  
                
        g3: if i/=29 generate 
           rx: entity work.regnbits
					generic map(EDGE=>'1')
					port map(ck=>ck, rst=>rst, ce=>wen(i), D=>DataWP, Q=>reg(i));                    
        end generate;
                   
   end generate g1;   
    

    DataRP1 <= reg(CONV_INTEGER(AdRP1));    -- source1 selection 

    DataRP2 <= reg(CONV_INTEGER(AdRP2));    -- source2 selection 
   
end reg_bank;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- ALU - Uma unidade logico-aritmetica puramente combinacional, cuja 
--		saida depende dos valores nas suas entradas de dados op1 e op2, cada
--		uma de 32 bits e da instrucao sendo executada pelo processador
--		que eh informada via o sinal de controle op_alu.
--
-- 22/11/2004 (Ney Calazans) - subtle error correction was done for J!
-- Part of the work for J has been done before, by shifting IR(15 downto 0)
-- left by two bits before writing data to register R3.
--
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.all;
use work.p_MIPS_Pipeline.all;

entity alu is
       port( op1, op2 : in std_logic_vector(31 downto 0);
             outalu :   out std_logic_vector(31 downto 0);   
             op_alu : in inst_type   
           );
end alu;

architecture alu of alu is 
   signal menorU, menorS : std_logic ;
begin
  
    menorU <=  '1' when op1 < op2 else '0';
    menorS <=  '1' when ieee.Std_Logic_signed."<"(op1,  op2) else '0' ; -- signed
    
    outalu <=  
        op1 - op2                            when  op_alu=SUBU                     else
        op1 and op2                          when  op_alu=AAND  or op_alu=ANDI     else 
        op1 or  op2                          when  op_alu=OOR   or op_alu=ORI      else 
        op1 xor op2                          when  op_alu=XXOR  or op_alu=XORI     else 
        op1 nor op2                          when  op_alu=NNOR                     else 
        op2(15 downto 0) & x"0000"           when  op_alu=LUI                      else
        (0=>menorU, others=>'0')             when  op_alu=SLTU  or op_alu=SLTIU    else
        (0=>menorS, others=>'0')             when  op_alu=SLT   or op_alu=SLTI     else
        op1(31 downto 28) & op2(27 downto 0) when  op_alu=J     or op_alu=JAL      else 
        op1                                  when  op_alu=JR    or op_alu=JALR     else 
        to_StdLogicVector(to_bitvector(op1) sll  CONV_INTEGER(op2(10 downto 6)))   when
													op_alu=SSLL   else 
        to_StdLogicVector(to_bitvector(op2) sll  CONV_INTEGER(op1(5 downto 0)))    when
													op_alu=SLLV   else 
        to_StdLogicVector(to_bitvector(op1) sra  CONV_INTEGER(op2(10 downto 6)))   when  
													op_alu=SSRA   else 
        to_StdLogicVector(to_bitvector(op2) sra  CONV_INTEGER(op1(5 downto 0)))    when  
													op_alu=SRAV   else 
        to_StdLogicVector(to_bitvector(op1) srl  CONV_INTEGER(op2(10 downto 6)))   when  
													op_alu=SSRL   else 
        to_StdLogicVector(to_bitvector(op2) srl  CONV_INTEGER(op1(5 downto 0)))    when
													op_alu=SRLV   else 
        op1 + op2;    -- default for ADDU,ADDIU,LBU,LW,SW,SB,BEQ,BGEZ,BLEZ,BNE,NOP   

end alu;

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Descricao Estrutural do Bloco de Dados 
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_signed.all; -- needed for comparison instructions SLTx
use IEEE.Std_Logic_arith.all; -- needed for comparison instructions SLTxU
use work.p_MIPS_Pipeline.all;
   
entity datapath is
      port(  ck, rst :     in std_logic;
				 ce, rw, bw : out std_logic; -- used in the forth stage to control memory access
             d_address :   out std_logic_vector(31 downto 0);
             data :        inout std_logic_vector(31 downto 0); 
             uins :        in microinstruction;
             IR_OUT :  		out std_logic_vector(31 downto 0);
				 i_address :   out std_logic_vector(31 downto 0);
				 instruction : in std_logic_vector(31 downto 0)
          );
end datapath;

architecture datapath of datapath is
    signal pc, incpc, IR, result, R1, R2, RB, RIN, sign_extend, op1, op2, RALU, mdr_int, 
				dtpc, cte_im: std_logic_vector(31 downto 0) := (others=> '0');
    signal adD, adS : std_logic_vector(4 downto 0) := (others=> '0');
    signal salta : std_logic := '0';
	 signal conflict : DataConflict;
	 
	 -- Dynamic Prediction Signals
	 constant array_range : integer:=31;
	 type buff is array(0 to array_range) of DynamicPrediction; 
	 constant initialize_array : DynamicPrediction := (VB => '0',
														FSM => (others=>'0'),
													address => (others=>'0'),
													b_address => (others=>'0'));
	 signal branch_buffer : buff := (others => initialize_array);
	 signal id_tb, id_wtb : integer :=0;
	 signal taken_branch : std_logic := '0';
	 signal wrongly_taken_branch : std_logic := '0';
	 
	 -- Pipeline Registers
	 signal bidi : BIDI;
	 signal diex : DIEX;
	 signal exmem : EXMEM;
	 signal memer : MEMER;
	 
begin

  --==============================================================================
   -- Data Conflict Unit
   --==============================================================================
	
	-------------------------------------------------------------------------------------------------
   -- opA  | Meaning - adD = RS
   -------------------------------------------------------------------------------------------------
	-- "00" | No conflict
	-- "01" | Instruction on the forth stage conflicts with the instruction on the second stage
	-- "10" | Instruction on the third stage conflicts with the instruction on the second stage
	-- "11" | Load word/Immediate instructions conflict
	-------------------------------------------------------------------------------------------------
	conflict.opA <= "11" when ((memer.uins.ce = '1') and (memer.uins.rw = '1') and (memer.uins.wreg = '1') and (memer.adD = diex.RS)) else
						 "10" when (exmem.uins.wreg = '1') and (exmem.adD /= "00000") and (exmem.adD = diex.RS) else 
						 "01" when (memer.uins.wreg = '1') and (memer.adD /= "00000") and (exmem.adD /= diex.RS) and (memer.adD = diex.RS) else
						 "00";
	
	-------------------------------------------------------------------------------------------------
   -- opB  | Meaning - adD = Rt
   -------------------------------------------------------------------------------------------------
	-- "00" | No conflict
	-- "01" | Instruction on the forth stage conflicts with the instruction on the second stage
	-- "10" | Instruction on the third stage conflicts with the instruction on the second stage
	-- "11" | Load word/Immediate instructions conflict
	-------------------------------------------------------------------------------------------------
	conflict.opB <= "11" when (memer.uins.ce = '1') and (memer.uins.rw = '1') and (memer.uins.wreg = '1') and (memer.adD = diex.RT) and
											(diex.uins.i /= LBU) and (diex.uins.i /= LW) and (diex.uins.i /= LUI) else
						 "10" when (exmem.uins.wreg = '1') and (exmem.adD /= "00000") and (exmem.adD = diex.RT) and 
										(diex.uins.i /= LBU) and (diex.uins.i /= LW) and (diex.uins.i /= LUI) else
						 "01" when (memer.uins.wreg = '1') and (memer.adD /= "00000") and (exmem.adD /= diex.RT) and (memer.adD = diex.RT) and 
										(diex.uins.i /= LUI) and (diex.uins.i /= LBU) and (diex.uins.i /= LW) else
						 "00";
						 
	-------------------------------------------------------------------------------------------------
   -- bubble | Meaning - Detects load word/Immediate instructions conflict, stops Pipeline for a clock cycle
   -------------------------------------------------------------------------------------------------
	-- '1' | Conflict
	-- '0' | No conflict
	-------------------------------------------------------------------------------------------------
	
	conflict.bubble <= '1' when (((diex.uins.ce = '1') and (diex.uins.rw = '1') and (diex.uins.wreg = '1') and (diex.RT = bidi.IR(20 downto 16))) 
										or ((diex.uins.ce = '1') and (diex.uins.rw = '1') and (diex.uins.wreg = '1') and (diex.RT = bidi.IR(25 downto 21))))
							else '0';
							
							


	--==============================================================================							
	-- Dynamic Prediction Unit
	--==============================================================================
	
	-------------------------------------------------------------------------------------------------
   -- Finite State Machine definition
   -------------------------------------------------------------------------------------------------
	-- "00" | Not taken
	-- "01" | Not taken
	-- "10" | Taken
	-- "11" | Taken
	-------------------------------------------------------------------------------------------------
	
	process(ck, rst)
	begin
		if (rst = '1') then
			branch_buffer <= (others => initialize_array);		
		elsif ck'event and ck = '1' then
			if (diex.uins.inst_branch='1') then
				for i in 0 to array_range loop
					if (branch_buffer(i).address = diex.pc) then
						if (branch_buffer(i).FSM = "00" and salta = '1') then
							branch_buffer(i).FSM <= "01";
							exit;
						elsif (branch_buffer(i).FSM = "01" and salta = '1') then
							branch_buffer(i).FSM <= "11" after 20ns;
							exit;
						elsif (branch_buffer(i).FSM = "01" and salta = '0') then
							branch_buffer(i).FSM <= "00";
							exit;
						elsif (branch_buffer(i).FSM = "11" and salta = '1') then
							branch_buffer(i).FSM <= "11";
							exit;
						elsif (branch_buffer(i).FSM = "11" and salta = '0') then
							branch_buffer(i).FSM <= "10";
							exit;
						elsif (branch_buffer(i).FSM = "10" and salta = '1') then
							branch_buffer(i).FSM <= "11" after 20ns;
							exit;
						elsif (branch_buffer(i).FSM = "10" and salta = '0') then
							branch_buffer(i).FSM <= "00" after 20ns;
						else	
							branch_buffer(i).FSM <= "00";
							exit;
						end if;
					elsif (branch_buffer(i).VB = '0') then
						branch_buffer(i).VB <= '1';
						branch_buffer(i).address <= diex.pc;
						branch_buffer(i).b_address <= RALU;
						exit;
					end if;
				end loop;
			end if;
		end if;
	end process;
	
	-------------------------------------------------------------------------------------------------
   -- Taken branch flag (FSM = "11" or FSM = "10")
   -------------------------------------------------------------------------------------------------
	
	process(ck)
	begin
		if ck'event and ck = '1' then
			for i in 0 to array_range loop
				if (branch_buffer(i).address = incpc) then
					if (branch_buffer(i).FSM = "11" or branch_buffer(i).FSM = "10") then
						id_tb <= i;
						taken_branch <= '1';
						taken_branch <= '0' after 20ns;
						exit;
					else
						id_tb <= 0;
						taken_branch <= '0';
						exit;
					end if;
				end if;
			end loop;
		end if;
	end process;
	
	-------------------------------------------------------------------------------------------------
   -- Wrongly taken branch flag ((FSM = "11" or FSM = "10") and salta = '0')
   -------------------------------------------------------------------------------------------------	
	process(ck)
	begin
		if ck'event and ck = '0' then
			for i in 0 to array_range loop
				if (branch_buffer(i).address = diex.pc) then
					if ((branch_buffer(i).FSM = "10" or branch_buffer(i).FSM = "11") and salta = '0') then
						id_wtb <= i;
						wrongly_taken_branch <= '1';
						wrongly_taken_branch <= '0' after 20ns;
						exit;
					else
						id_wtb <= 0;
						wrongly_taken_branch <= '0';
						exit;
					end if;
				end if;
			end loop;
		end if;
	end process;
	
	
	
   --==============================================================================
   -- first stage
   --==============================================================================
      -- PC Register
		-- Code memory starting address: beware of the OFFSET!
		-- The one below (x"00400000") serves for code generated 
      -- by the MARS simulator
		-- The signal ce is always "on", because it doesn't depend on it, just the clock signal

	RPC: entity work.regnbits generic map(INIT_VALUE=>x"00400000")   
                            port map(ck=>ck, rst=>rst, ce=>'1', D=>dtpc, Q=>pc);
   
	incpc <= branch_buffer(id_tb).b_address when (taken_branch = '1')
				else (branch_buffer(id_wtb).address + 4) when (wrongly_taken_branch = '1')
				else pc + 4;    
				
   i_address <= pc; -- connects PC output to the instruction memory address bus
	
	dtpc <= exmem.RALU when (exmem.uins.inst_branch='1' and exmem.salta='1' and taken_branch = '0') or
									exmem.uins.i=J or exmem.uins.i=JAL or exmem.uins.i=JALR or exmem.uins.i=JR
				else pc when (conflict.bubble = '1')
				else incpc;
				
	
	--==============================================================================
   -- BI/DI Pipeline Register
   --==============================================================================
	process(ck, rst)
	begin
		if (rst = '1') then
			 bidi.npc <= (others=>'0');
			 bidi.IR <= (others=>'0');
			 bidi.pc <= (others=>'0');
		elsif ck'event and ck = '0' then
			if (diex.uins.inst_branch='1') then
				for i in 0 to array_range loop
					if (branch_buffer(i).address = diex.pc) then
						if ((branch_buffer(i).FSM = "00" or branch_buffer(i).FSM = "01") and salta = '1') then
							bidi.npc <= bidi.npc;
							bidi.IR <= (others=>'0');
							bidi.pc <= (others=>'0');
							exit;
						elsif ((branch_buffer(i).FSM = "10" or branch_buffer(i).FSM = "11") and salta = '0') then
							bidi.npc <= bidi.npc;
							bidi.IR <= (others=>'0');
							bidi.pc <= (others=>'0');
							exit;
						elsif (conflict.bubble = '1') then
							bidi.npc <= bidi.npc;
							bidi.IR <= bidi.IR;
							bidi.pc <= bidi.pc;
							exit;
						else
							bidi.npc <= incpc; -- store in the pipeline register pc + 4
							bidi.IR <= instruction; -- store in the pipeline register instruction code
							bidi.pc <= pc;
							exit;
						end if;
					end if;
				end loop;
			elsif (diex.uins.i = J or diex.uins.i = JAL) then
				bidi.npc <= bidi.npc;
				bidi.IR <= (others=>'0');
				bidi.pc <= (others=>'0');
			elsif (conflict.bubble = '1') then
				bidi.npc <= bidi.npc;
				bidi.IR <= bidi.IR;
				bidi.pc <= bidi.pc;
			else
				bidi.npc <= incpc; -- store in the pipeline register pc + 4
				bidi.IR <= instruction; -- store in the pipeline register instruction code
				bidi.pc <= pc;
			end if;
		end if;
	end process;

   --==============================================================================
   -- second stage
   --==============================================================================
   
	IR_OUT <= bidi.IR; 	-- IR is the datapath output signal to carry the instruction
	IR <= bidi.IR;
	
   -- The then clause is only used for logic shifts with a shamt field       
   adS <= IR(20 downto 16) when uins.i=SSLL or uins.i=SSRA or uins.i=SSRL else 
          IR(25 downto 21);
          
   REGS: entity work.reg_bank(reg_bank) port map
        (AdRP1=>adS, DataRP1=>R1, AdRP2=>IR(20 downto 16), DataRP2=>R2,
		   ck=>ck, rst=>rst, ce=>memer.uins.wreg, AdWP=>memer.adD, DataWP=>RIN);
    
   -- sign extension 
   sign_extend <=  x"FFFF" & IR(15 downto 0) when IR(15)='1' else
             x"0000" & IR(15 downto 0);
    
   -- Immediate constant
   cte_im <= sign_extend(29 downto 0)  & "00"     when uins.inst_branch='1' else
                -- branch address adjustment for word frontier
             "0000" & IR(25 downto 0) & "00" when uins.i=J or uins.i=JAL else
                -- J/JAL are word addressed. MSB four bits are defined at the ALU, not here!
             x"0000" & IR(15 downto 0) when uins.i=ANDI or uins.i=ORI  or uins.i=XORI else
                -- logic instructions with immediate operand are zero extended
             sign_extend;
                -- The default case is used by addiu, lbu, lw, sbu and sw instructions
					 

	--==============================================================================
   -- DI/EX Pipeline Register
   --==============================================================================
 
	--process(ck, rst)
	process(ck, rst)
	begin
		if (rst = '1') then
			diex.npc <= (others=>'0');
			diex.pc <= (others=>'0');
			diex.RD <= (others=>'0');
			diex.RT <= (others=>'0');
			diex.RS <= (others=>'0');
			diex.RA <= (others=>'0');
			diex.RB <= (others=>'0');
			diex.IMED <= (others=>'0');
			-- uins rst
			diex.uins.inst_branch <= '0';
			diex.uins.inst_grupo1 <= '0';
			diex.uins.inst_grupoI <= '0';
		diex.uins.wreg <= '0';
		diex.uins.ce <= '0';
		diex.uins.rw <= '0';
		diex.uins.bw <= '1';
		diex.uins.i <= NOP;
		
		elsif ck'event and ck = '0' then
			if (diex.uins.inst_branch='1') then
				for i in 0 to array_range loop
					if (branch_buffer(i).address = diex.pc) then
						if ((branch_buffer(i).FSM = "00" or branch_buffer(i).FSM = "01") and salta = '1') then
							diex.npc <= (others=>'0');
							diex.pc <= (others=>'0');
							diex.RD <= (others=>'0');
							diex.RT <= (others=>'0');
							diex.RS <= (others=>'0');
							diex.RA <= (others=>'0');
							diex.RB <= (others=>'0');
							diex.IMED <= (others=>'0');
							-- uins rst
							diex.uins.inst_branch <= '0';
							diex.uins.inst_grupo1 <= '0';
							diex.uins.inst_grupoI <= '0';
							diex.uins.wreg <= '0';
							diex.uins.ce <= '0';
							diex.uins.rw <= '0';
							diex.uins.bw <= '1';
							diex.uins.i <= NOP;
							exit;
						elsif ((branch_buffer(i).FSM = "10" or branch_buffer(i).FSM = "11") and salta = '0')  then
							diex.npc <= (others=>'0');
							diex.pc <= (others=>'0');
							diex.RD <= (others=>'0');
							diex.RT <= (others=>'0');
							diex.RS <= (others=>'0');
							diex.RA <= (others=>'0');
							diex.RB <= (others=>'0');
							diex.IMED <= (others=>'0');
							-- uins rst
							diex.uins.inst_branch <= '0';
							diex.uins.inst_grupo1 <= '0';
							diex.uins.inst_grupoI <= '0';
							diex.uins.wreg <= '0';
							diex.uins.ce <= '0';
							diex.uins.rw <= '0';
							diex.uins.bw <= '1';
							diex.uins.i <= NOP;
							exit;
						elsif (conflict.bubble = '1' or diex.uins.i = J or diex.uins.i = JAL) then
							diex.npc <= (others=>'0');
							diex.pc <= (others=>'0');
							diex.RD <= (others=>'0');
							diex.RT <= (others=>'0');
							diex.RS <= (others=>'0');
							diex.RA <= (others=>'0');
							diex.RB <= (others=>'0');
							diex.IMED <= (others=>'0');
							-- uins rst
							diex.uins.inst_branch <= '0';
							diex.uins.inst_grupo1 <= '0';
							diex.uins.inst_grupoI <= '0';
							diex.uins.wreg <= '0';
							diex.uins.ce <= '0';
							diex.uins.rw <= '0';
							diex.uins.bw <= '1';
							diex.uins.i <= NOP;
							exit;
						else
							diex.uins <= uins;
							diex.npc <= bidi.npc;
							diex.pc <= bidi.pc;
							diex.RD <= IR(15 downto 11);
							diex.RT <= IR(20 downto 16);
							diex.RS <= IR(25 downto 21);
							diex.IMED <= cte_im;
							diex.RA <= R1;
							diex.RB <= R2;
							exit;
						end if;
					end if;
				end loop;
			elsif (conflict.bubble = '1' or diex.uins.i = J or diex.uins.i = JAL) then
				diex.npc <= (others=>'0');
				diex.pc <= (others=>'0');
				diex.RD <= (others=>'0');
				diex.RT <= (others=>'0');
				diex.RS <= (others=>'0');
				diex.RA <= (others=>'0');
				diex.RB <= (others=>'0');
				diex.IMED <= (others=>'0');
				-- uins rst
				diex.uins.inst_branch <= '0';
				diex.uins.inst_grupo1 <= '0';
				diex.uins.inst_grupoI <= '0';
				diex.uins.wreg <= '0';
				diex.uins.ce <= '0';
				diex.uins.rw <= '0';
				diex.uins.bw <= '1';
				diex.uins.i <= NOP;	
			else
				diex.uins <= uins;
				diex.npc <= bidi.npc;
				diex.pc <= bidi.pc;
				diex.RD <= IR(15 downto 11);
				diex.RT <= IR(20 downto 16);
				diex.RS <= IR(25 downto 21);
				diex.IMED <= cte_im;
				diex.RA <= R1;
				diex.RB <= R2;
			end if;
		end if;
	end process;
			
	
  --==============================================================================
  -- third stage
  --==============================================================================
  
					
					-- select the first ALU operand
   op1 <= 	diex.npc  when (diex.uins.inst_branch='1' 
					or diex.uins.i=J or diex.uins.i=JAL) else 
				exmem.RALU when ((conflict.opA = "10") and (diex.uins.inst_branch='0')) else
				memer.RALU when ((conflict.opA = "01") and (diex.uins.inst_branch='0')) else
				memer.MDR when (conflict.opA = "11") else
				diex.RA; 
				
				   -- select the second ALU operand
   op2 <=   diex.RB when (diex.uins.inst_grupo1='1' or diex.uins.i=SLTU or diex.uins.i=SLT or diex.uins.i=JR 
                  or diex.uins.i=SLLV or diex.uins.i=SRAV or diex.uins.i=SRLV) and (conflict.opB = "00") else 
				exmem.RALU when ((conflict.opB = "10") and (diex.uins.inst_branch='0') and (diex.uins.i /= SW) and (diex.uins.inst_grupoI = '0')) else
				memer.RALU when ((conflict.opB = "01") and (diex.uins.inst_branch='0') and (diex.uins.i /= SW) and (diex.uins.inst_grupoI = '0')) else
				memer.MDR when ((conflict.opB = "11") and (diex.uins.i /= SW) and (diex.uins.inst_grupoI = '0')) else
          	diex.IMED; 
				
					-- saves RT register's content
	RB  <=	memer.MDR when (conflict.opB = "11") else
				exmem.RALU when ((conflict.opB = "10") and (diex.uins.inst_branch='0')) else
				memer.RALU when ((conflict.opB = "01") and (diex.uins.inst_branch='0')) else
				diex.RB;
				
                 
   -- ALU instantiation
   DALU: entity work.alu port map (op1=>op1, op2=>op2, outalu=>RALU, op_alu=>diex.uins.i);
            
 
    -- evaluation of conditions to take the branch instructions
   salta <=  '1' when ( 
								(conflict.opA = "10" and diex.RB=exmem.RALU and diex.uins.i=BEQ) or
								(conflict.opB = "10" and diex.RA=exmem.RALU and diex.uins.i=BEQ) or
								(conflict.opA = "01" and diex.RB=memer.RALU and diex.uins.i=BEQ) or
								(conflict.opB = "01" and diex.RA=memer.RALU and diex.uins.i=BEQ) or
								(conflict.opA = "00" and conflict.opB = "00" and diex.RA=diex.RB  and diex.uins.i=BEQ) or
								(conflict.opA = "10" and exmem.RALU>=0 and diex.uins.i=BGEZ) or
								(conflict.opA = "01" and memer.RALU>=0 and diex.uins.i=BGEZ) or
								(conflict.opA = "00" and diex.RA>=0 and diex.uins.i=BGEZ) or
								(conflict.opA = "10" and exmem.RALU<=0 and diex.uins.i=BLEZ) or
								(conflict.opA = "01" and memer.RALU<=0 and diex.uins.i=BLEZ) or
								(conflict.opA = "00" and diex.RA<=0 and diex.uins.i=BLEZ) or
								(conflict.opA = "10" and diex.RB/=exmem.RALU and diex.uins.i=BNE) or
								(conflict.opB = "10" and diex.RA/=exmem.RALU and diex.uins.i=BNE) or
								(conflict.opA = "01" and diex.RB/=memer.RALU and diex.uins.i=BNE) or
								(conflict.opB = "01" and diex.RA/=memer.RALU and diex.uins.i=BNE) or
								(conflict.opA = "00" and conflict.opB = "00" and diex.RA/=diex.RB and diex.uins.i=BNE) 
								)
					else '0';
	
 
	 -- register bank write address selection
   adD <= "11111"           when diex.uins.i=JAL else -- JAL writes in register $31
         diex.RD    when (diex.uins.inst_grupo1='1' 
					or diex.uins.i=SLTU or diex.uins.i=SLT
					or diex.uins.i=JALR
					or diex.uins.i=SSLL or diex.uins.i=SLLV
					or diex.uins.i=SSRA or diex.uins.i=SRAV
					or diex.uins.i=SSRL or diex.uins.i=SRLV) else
         diex.RT 	-- inst_grupoI='1' or uins.i=SLTIU or uins.i=SLTI 
        ;                 		-- or uins.i=LW or  uins.i=LBU  or uins.i=LUI, or default
    
	--==============================================================================
	-- EX/MEM Pipeline Register
	--==============================================================================
  
	process(ck, rst)
	begin
		 if rst = '1' then
			  exmem.npc <= (others=>'0');
			  exmem.salta <= '0';
			  exmem.RALU <= (others=>'0');
			  exmem.adD <= (others=>'0');
			  exmem.RB <= (others=>'0');
			  -- uins rst
			  exmem.uins.inst_branch <= '0';
			  exmem.uins.inst_grupo1 <= '0';
			  exmem.uins.inst_grupoI <= '0';
			  exmem.uins.wreg <= '0';
			  exmem.uins.ce <= '0';
			  exmem.uins.rw <= '0';
			  exmem.uins.bw <= '1';
			  exmem.uins.i <= NOP;
			  
		 elsif ck'event and ck = '0' then
			  exmem.npc <= diex.npc;
			  exmem.salta <= salta;
			  exmem.RALU <= RALU;
			  exmem.adD <= adD;
			  exmem.uins <= diex.uins;
			  exmem.RB <= RB;
		 end if;
	end process;	

  
   --==============================================================================
   -- fourth stage
   --==============================================================================
     
	ce <= exmem.uins.ce;
   rw <= exmem.uins.rw;
	bw <= exmem.uins.bw;
	
   d_address <= exmem.RALU;
    
   -- tristate to control memory write    
   data <= exmem.RB when (exmem.uins.ce='1' and exmem.uins.rw='0') else (others=>'Z');  

   -- single byte reading from memory  -- assuming the processor is little endian
   mdr_int <= data when exmem.uins.i=LW else
              x"000000" & data(7 downto 0);

	--==============================================================================
	-- MEM/ER Pipeline Register
	--==============================================================================
  
	process(ck, rst)
	begin
		 if rst = '1' then
			  memer.npc <= (others=>'0');
			  memer.RALU <= (others=>'0');
			  memer.adD <= (others=>'0');
			  memer.MDR <= (others=>'0');
			  -- uins rst
			  memer.uins.inst_branch <= '0';
			  memer.uins.inst_grupo1 <= '0';
			  memer.uins.inst_grupoI <= '0';
			  memer.uins.wreg <= '0';
			  memer.uins.ce <= '0';
			  memer.uins.rw <= '0';
			  memer.uins.bw <= '1';
			  memer.uins.i <= NOP;
			  
		 elsif ck'event and ck = '0' then
			  memer.npc <= exmem.npc;
			  memer.RALU <= exmem.RALU;
			  memer.adD <= exmem.adD;
			  memer.MDR <= mdr_int;
			  memer.uins <= exmem.uins;
		 end if;
	end process;	

   --==============================================================================
   -- fifth stage
   --==============================================================================
		
	result <=	memer.MDR when memer.uins.i=LW  or memer.uins.i=LBU else
               memer.RALU;
		
   -- signal to be written into the register bank
   RIN <= memer.npc when (memer.uins.i=JALR or memer.uins.i=JAL) else result;

	 
end datapath;

--------------------------------------------------------------------------
--------------------------------------------------------------------------
--  Descricao do Bloco de Controle (mista, estrutural-comportamental)
--------------------------------------------------------------------------
--------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use IEEE.Std_Logic_unsigned.all;
use work.p_MIPS_Pipeline.all;

entity control_unit is
        port(	ck, rst : in std_logic;
					uins : out microinstruction;
					ir: in std_logic_vector(31 downto 0)
             );
end control_unit;
                   
architecture control_unit of control_unit is
   signal i : inst_type;
begin
   
    ----------------------------------------------------------------------------------------
    -- BLOCK (1/2) - INSTRUCTION DECODING and ALU operation definition.
    -- This block generates one signal (i) of the Control Unit Output Function
    ----------------------------------------------------------------------------------------
    i <=   ADDU   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100001" else
           SUBU   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100011" else
           AAND   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100100" else
           OOR    when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100101" else
           XXOR   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100110" else
           NNOR   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000100111" else
           SSLL   when IR(31 downto 21)="00000000000" and IR(5 downto 0)="000000"
													and IR(25 downto 9) /= "00000000000000000" else
           SLLV   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000000100" else
           SSRA   when IR(31 downto 21)="00000000000" and IR(5 downto 0)="000011" else
           SRAV   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000000111" else
           SSRL   when IR(31 downto 21)="00000000000" and IR(5 downto 0)="000010" else
           SRLV   when IR(31 downto 26)="000000" and IR(10 downto 0)="00000000110" else
           ADDIU  when IR(31 downto 26)="001001" else
           ANDI   when IR(31 downto 26)="001100" else
           ORI    when IR(31 downto 26)="001101" else
           XORI   when IR(31 downto 26)="001110" else
           LUI    when IR(31 downto 26)="001111" else
           LW     when IR(31 downto 26)="100011" else
           LBU    when IR(31 downto 26)="100100" else
           SW     when IR(31 downto 26)="101011" else
           SB     when IR(31 downto 26)="101000" else
           SLTU   when IR(31 downto 26)="000000" and IR(5 downto 0)="101011" else
           SLT    when IR(31 downto 26)="000000" and IR(5 downto 0)="101010" else
           SLTIU  when IR(31 downto 26)="001011"                             else
           SLTI   when IR(31 downto 26)="001010"                             else
           BEQ    when IR(31 downto 26)="000100" else
           BGEZ   when IR(31 downto 26)="000001" and IR(20 downto 16)="00001" else
           BLEZ   when IR(31 downto 26)="000110" and IR(20 downto 16)="00000" else
           BNE    when IR(31 downto 26)="000101" else
           J      when IR(31 downto 26)="000010" else
           JAL    when IR(31 downto 26)="000011" else
           JALR   when IR(31 downto 26)="000000"  and IR(20 downto 16)="00000"
                                           and IR(10 downto 0) = "00000001001" else
           JR     when IR(31 downto 26)="000000" and IR(20 downto 0)="000000000000000001000" else
			  NOP		when IR(31 downto 0)=x"00000000" else
           invalid_instruction ; -- IMPORTANT: default condition is invalid instruction;
        
    assert i /= invalid_instruction
          report "******************* INVALID INSTRUCTION *************"
          severity error;
                   
    uins.i <= i;    -- this instructs the alu to execute its expected operation, if any

    ----------------------------------------------------------------------------------------
    -- BLOCK (2/2) - DATAPATH REGISTERS load control signals generation.
    ----------------------------------------------------------------------------------------
   
	 -- auxiliary signals
	 uins.inst_branch <= '1' when i=BEQ or i=BGEZ or i=BLEZ or i=BNE else '0';
	 
	 uins.inst_grupo1 <= '1' when i=ADDU or i=SUBU or i=AAND or i=OOR
										or i=XXOR or i=NNOR else '0';
	
	 uins.inst_grupoI <= '1' when i=ADDIU or i=ANDI or i=ORI or i=XORI else '0';

	 -- write register bank enable 
	 uins.wreg <= '1' when (i/=SB and i/=SW and i/=JR and i/=J and i/=BEQ and i/=BGEZ
										and i/=BLEZ and i/=BNE and i/=NOP and i/=invalid_instruction)
							else '0';

	 -- memory operation enable and acess configuration
	 uins.ce <= '1' when (i=LBU or i=LW) or (i=SB or i=SW) else '0';
		
	 uins.rw <= '0' when (i=SB or i=SW) else '1';
		
	 uins.bw <= '0' when i=SB else '1';
		
	 
end control_unit;

--------------------------------------------------------------------------
-- Processador MIPS_Pipeline completo, onde se instanciam BD e BC
--------------------------------------------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_MIPS_Pipeline.all;

entity MIPS_Pipeline is
    port( clock, reset: in std_logic;
          ce, rw, bw: out std_logic;
          i_address, d_address: out std_logic_vector(31 downto 0);
          instruction: in std_logic_vector(31 downto 0);
          data: inout std_logic_vector(31 downto 0));
end MIPS_Pipeline;

	architecture MIPS_Pipeline of MIPS_Pipeline is
      signal IR: std_logic_vector(31 downto 0);
      signal uins: microinstruction;  
	   signal memCE, memRW, memBW: std_logic;
 begin

     dp: entity work.datapath   
         port map(ck=>clock, rst=>reset, d_address=>d_address, data=>data,
						i_address=>i_address, instruction=>instruction, uins=>uins, 
						IR_OUT=>IR, ce=>memCE, rw=>memRW, bw=>memBW);

     ct: entity work.control_unit port map( ck=>clock, rst=>reset, uins=>uins, IR=>IR);
         
     ce <= memCE;
     rw <= memRW; 
     bw <= memBW;
     
end MIPS_Pipeline;

