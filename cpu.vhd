-- Entity RAM:
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

entity RAM is
  port(
   CLK   : in std_logic;                            -- hodinovy signal
   DATA_ADDR  : in std_logic_vector(12 downto 0);   -- adresa do pameti
   DATA_WDATA : in std_logic_vector(7 downto 0);    -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : out std_logic_vector(7 downto 0);   -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : in std_logic;                       -- cteni (0) / zapis (1)
   DATA_EN    : in std_logic                        -- povoleni cinnosti 
  );
  end RAM;

  architecture behavioral of RAM is
    type t_ram_data is array (0 to 2**13-1) of std_logic_vector(7 downto 0);
    signal ram_data : t_ram_data;
  begin
  
   Zapis_dat: process(CLK, DATA_EN, DATA_RDWR, DATA_ADDR, DATA_WDATA)
   begin
    if (CLK'event) and (CLK = '1') then
      if (DATA_EN = '1') then
        if(DATA_RDWR = '1') then
          ram_data(conv_integer(DATA_ADDR(12 downto 0))) <= DATA_WDATA;
        end if;
        DATA_RDATA <= ram_data(conv_integer(DATA_ADDR(12 downto 0)));
      end if;
    end if;
   end process Zapis_dat;

  end behavioral;





-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): jmeno <login AT stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0);-- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

-- Data registr
signal data_reg : std_logic_vector(7 downto 0);
signal datareg_en : std_logic_vector (1 downto 0) := (others => '0');
-- Data mx
signal datareg_change : std_logic_vector (1 downto 0) := (others => '0');
-- Data mx
-- Data registr

--Program counter
signal pc_reg : std_logic_vector(12 downto 0)  := (others => '0');
signal pcreg_change : std_logic_vector(1 downto 0) := (others => '0');
--Program counter

-- Cycle counter
signal cycle_cnt_reg : std_logic_vector (12 downto 0) := (others => '0');
signal cycle_cnt_change : std_logic_vector (1 downto 0) := (others => '0');
-- Cycle counter

-- Instrukcni dekoder
type ins_type is (inc_pt, dec_pt, inc_data, dec_data, start_dowhile, start_while, end_while, print_data, scan_data, read_stop, pass);
--                   >       <        +         -           (             [        ] nebo )      .           ,         NULL  jina data   
signal ireg_decode : ins_type; -- IN_DATA decode
signal iregdecode_en : std_logic  := '0';
-- Instrukcni dekoder

-- Instrukcni registr
signal ireg_reg : std_logic_vector(7 downto 0); -- IN_DATA
signal iregreg_en : std_logic := '0';
-- Instrukcni registr

-- Adresovy registr
signal iar_reg : std_logic_vector(12 downto 0) := "1000000000000"; -- DATA_ADDR
signal iarreg_change : std_logic_vector(1 downto 0) := (others => '0');
-- Adresovy registr


-- FSM
  type type_fsm_state is (fsm_read_data, fsm_read_program_forward, fsm_read_program_back, fsm_inc_data, fsm_dec_data, fsm_write_data, fsm_iar_inc, fsm_iar_dec, fsm_start_while, fsm_end_while, fsm_start_dowhile, fsm_scan_data, fsm_print_data, fsm_end);
  signal  fsm_state : type_fsm_state := fsm_read_program_forward;
  signal fsm_voidcycle : std_logic := '0';
  signal fsm_wait : std_logic_vector(2 downto 0) := "000";
  signal fail : std_logic := '0';
-- FSM

begin

 
 -- Data registr
 Data_register: process(CLK, RESET, DATA_RDATA)
 begin
   if (RESET = '1') then
     data_reg <= (others => '0');
   elsif (CLK'event) and (CLK = '1') then
	  case(datareg_en) is
	   when "01" =>
	    data_reg <= DATA_RDATA; 
		 when "10" =>
		  data_reg <= IN_DATA; -- IN_DATA
		when others => 
	  end case;
     -- data mx
     case (datareg_change) is
      when "01" =>
        data_reg <= data_reg + 1;
      when "10" =>
        data_reg <= data_reg - 1;
      when others =>
     end case;
     -- data mx
   end if;
 end process Data_register;
 -- Data registr


-- Program counter (PC)
 PC: process(CLK, RESET)
 begin
  if (RESET = '1') then
    pc_reg <= (others=>'0'); 
  elsif (CLK'event) and (CLK = '1') then    
      case (pcreg_change) is
      when "01" => -- +
			if(pc_reg = "0111111111111") then
				pc_reg <= "0000000000000";
			else
				pc_reg <= pc_reg + 1;
			end if;
		when "10" => -- -
        if(pc_reg = "0000000000000") then
				pc_reg <= "0111111111111";
			else
				pc_reg <= pc_reg - 1;
			end if;
		 when "11" =>
			pc_reg <= "0000000000000";
		 when others =>
    end case;
	end if;
 end process PC;
 -- Program counter (PC)


  -- Instrukcni registr
 ireg: process(CLK, RESET, DATA_RDATA) -- Nactena instrukce
 begin
  if (RESET = '1') then
    ireg_reg <= (others=>'0');
  elsif (CLK'event) and (CLK = '1') then
   case(iregreg_en) is
    when '1' =>
      ireg_reg <= DATA_RDATA;
    when others =>
   end case;
	end if;
 end process ireg;
  -- Instrukcni registr


 -- Cycle counter
cyclecnt: process(CLK, RESET)
begin
  if (RESET = '1') then
    cycle_cnt_reg <= (others => '0');
  elsif (CLK'event) and (CLK = '1') then
    case(cycle_cnt_change) is
      when "01" =>
        cycle_cnt_reg <= cycle_cnt_reg + 1;
      when "10" =>
        cycle_cnt_reg <= cycle_cnt_reg - 1;
		  when "11" =>
			cycle_cnt_reg <= (others => '0');
      when others =>
		
		end case;
	end if;
end process cyclecnt;
 -- Cycle counter


   -- Adresovy registr (PTR)
 iar: process(CLK, RESET) -- Adresa v RAM
 begin
    if (RESET = '1') then
      iar_reg <= "1000000000000";
    elsif (CLK'event) and (CLK = '1') then
      case (iarreg_change) is
        when "01" =>
		   case(iar_reg) is
				when "1111111111111" =>
					iar_reg <= "1000000000000";
				when others =>
					iar_reg <= iar_reg + 1;
			end case;
		  when "10" =>
		   case(iar_reg) is 
				when "1000000000000" =>
					iar_reg <= "1111111111111";
				when others =>
					iar_reg <= iar_reg - 1;
			end case;
        when others =>
      end case;
    end if;
 end process iar;
  -- Adresovy registr (PTR)

  -- Instrukcni dekoder
 decoder: process(CLK, ireg_reg)
 begin
  -- if (CLK'event) and (CLK = '1') then
  if (iregdecode_en = '1') then
      case(ireg_reg) is 
          when "00111110" =>  -- >
            ireg_decode <= inc_pt;
          when "00111100" =>  -- <
            ireg_decode <= dec_pt;
          when "00101011" =>  -- +
            ireg_decode <= inc_data;  
          when "00101101" =>  -- -
            ireg_decode <= dec_data; 
          when "01011011" =>  -- [
            ireg_decode <= start_while; 
          when "01011101" =>  -- ]
            ireg_decode <= end_while; 
          when "00101000" =>  -- (
            ireg_decode <= start_dowhile;
          when "00101001" =>  -- )
            ireg_decode <= end_while;  
          when "00101110" =>  -- .
            ireg_decode <= print_data;
          when "00101100" =>  -- ,
            ireg_decode <= scan_data;
          when "00000000" =>  -- NULL
            ireg_decode <= read_stop;
          when others =>  
				ireg_decode <= pass; -- jina data
      end case;
  end if;
 end process decoder;
  -- Instrukcni dekoder

  
  -- FSM
FSM: process(CLK, RESET, EN, IN_VLD, OUT_BUSY)
begin
  if (RESET = '1') then
    DATA_EN <= '0';
    DATA_ADDR  <= (others => '0');
    DATA_WDATA <= (others => '0');
    DATA_RDWR  <= '0';
    IN_REQ <= '0';
    OUT_DATA <= (others => '0');
    OUT_WE   <= '0';
    fsm_state <= fsm_read_program_forward;
  elsif (CLK'event) and (CLK = '1') then
    if (EN = '1') then
      case(fsm_state) is

        when fsm_read_program_forward =>
          DATA_EN <= '1'; -- zapnu RAM 
          DATA_RDWR <= '0'; -- pouze cteni
          DATA_ADDR <= pc_reg; -- adresa v pameti
			    iregreg_en <= '1';
			    iregdecode_en <= '1';
          pcreg_change <= "00";
			    cycle_cnt_change <= "11"; -- cnt = 0
			    fsm_wait <= fsm_wait+1;
			    if(fsm_wait = "100") then 
			      fsm_wait <= "000";
				    iregreg_en <= '0';
				    iregdecode_en <= '0';
            case(ireg_decode) is 
              when inc_data =>    -- +
				        if(fsm_voidcycle = '0') then
					        fsm_state <= fsm_read_data;
				        end if;
				        pcreg_change <= "01";
                when dec_data =>    -- -
				          if(fsm_voidcycle = '0') then
				          fsm_state <= fsm_read_data;
				          end if;
				        pcreg_change <= "01";
                when inc_pt =>      -- >
				        if(fsm_voidcycle = '0') then
				          fsm_state <= fsm_iar_inc;
				        end if;
				        pcreg_change <= "01";
                when dec_pt =>      -- <
				          if(fsm_voidcycle = '0') then
				            fsm_state <= fsm_iar_dec;
				          end if;
				        pcreg_change <= "01";
                when print_data =>  -- .
				        if(fsm_voidcycle = '0') then
				          fsm_state <= fsm_read_data;
				        end if;
				        pcreg_change <= "01";
                when start_while => -- [
				        if(fsm_voidcycle = '0') then
				          fsm_state <= fsm_read_data;
				        end if;
				        pcreg_change <= "01";
                when start_dowhile => -- (
				        if(fsm_voidcycle = '0') then
				          fsm_state <= fsm_start_dowhile;
				        end if;
				        pcreg_change <= "01";
				        when end_while => -- ],)
				        if(fsm_voidcycle = '0') then
				          fsm_state <= fsm_end_while;
				        else
				          fsm_voidcycle <= '0';
				        end if;
				        pcreg_change <= "01";
				        when read_stop => -- null
					        fsm_state <= fsm_end;
				        when scan_data =>
					      pcreg_change <= "01";
                IN_REQ <= '1';
					      fsm_state <= fsm_scan_data;
				      when others => -- jiny znak
				        pcreg_change <= "01";
				      end case;
          end if;


        when fsm_read_data =>
			    pcreg_change <= "00";
          DATA_EN <= '1'; -- zapnu RAM 
          DATA_RDWR <= '0'; -- pouze cteni
          DATA_ADDR <= iar_reg; -- adresa v pameti
			    fsm_wait <= fsm_wait+1; 
			    if(fsm_wait = "010") then --01 
			      fsm_wait <= "000"; 
          case(ireg_decode) is
            when inc_data => -- +
				      datareg_en <= "01"; -- zapis do registru dat
              fsm_state <= fsm_inc_data;
            when dec_data => -- -
              datareg_en <= "01"; -- zapis do registru dat
				  fsm_state <= fsm_dec_data;
            when print_data => -- .
              datareg_en <= "01"; -- zapis do registru dat
              fsm_state <= fsm_print_data;
            when start_while => -- [
              datareg_en <= "01"; -- zapis do registru dat
              fsm_state <= fsm_start_while;
            when others =>
          
          end case;
			 end if;


      when fsm_print_data =>
      fsm_wait <= fsm_wait + 1;
      if(fsm_wait = "001") then

        if(OUT_BUSY = '0') then
          OUT_DATA <= data_reg;
          OUT_WE <= '1';
          fsm_wait <= "000";
          fsm_state <= fsm_end;
        end if;
      end if;
       
			  
		  when fsm_scan_data =>
			 pcreg_change <= "00";
			 if(IN_VLD = '1') then
				datareg_en <= "10";
				fsm_wait <= fsm_wait + 1;
				if(fsm_wait = "001") then
					IN_REQ <= '0';
					fsm_wait <= "000";
					datareg_en <= "00";
					fsm_state <= fsm_write_data;
				end if;
			 end if;

        when fsm_end_while =>
		      pcreg_change <= "00";
          datareg_en <= "00"; -- konec zapisu do registru dat
            if(data_reg = "00000000") then 
            fsm_state <= fsm_end;
            else
            fsm_state <= fsm_read_program_back;
            end if;

        when fsm_read_program_back =>
          DATA_EN <= '1'; -- zapnu RAM 
          DATA_RDWR <= '0'; -- pouze cteni
          DATA_ADDR <= pc_reg; -- adresa v pameti
			    iregreg_en <= '1';
			    iregdecode_en <= '1';
          pcreg_change <= "00";
			    cycle_cnt_change <= "00";
          fsm_wait <= fsm_wait+1;
			    if(fsm_wait = "011") then 
			      fsm_wait <= "000";
				    iregreg_en <= '0';
				    iregdecode_en <= '0';
				    case(ireg_decode) is
					    when start_while => -- [
						    if(cycle_cnt_reg = "0000000000001") then
							    fsm_state <= fsm_read_program_forward;
						    else
							    pcreg_change <= "10";
						    end if;
						    cycle_cnt_change <= "10"; -- cnt - 1
					    when start_dowhile => -- (
						    if(cycle_cnt_reg = "0000000000001") then
							    fsm_state <= fsm_read_program_forward;
						    else
							    pcreg_change <= "10";
						    end if;
						    cycle_cnt_change <= "10"; -- cnt - 1
					    when end_while => -- ] nebo )
						    if(pc_reg = "0000000000000") then
						      fsm_state <= fsm_end;
						      fail <= '1';
						    else
							    cycle_cnt_change <= "01"; -- cnt + 1
							    pcreg_change <= "10";
						    end if;
					    when others =>
						    if(pc_reg = "0000000000000") then
						      fsm_state <= fsm_end;
						      fail <= '1';
						    else
						      pcreg_change <= "10";
						    end if;
				    end case;
			    end if;
        

        when fsm_start_while =>
			    pcreg_change <= "00";
          datareg_en <= "00"; -- konec zapisu do registru dat
          fsm_wait <= fsm_wait + 1;
          if(fsm_wait = "001") then 
            fsm_wait <= "000";
            if(data_reg = "00000000") then 
              fsm_voidcycle <= '1';
            end if;
            fsm_state <= fsm_read_program_forward;
          end if;

        when fsm_start_dowhile =>
			    pcreg_change <= "00";
          datareg_en <= "00"; -- konec zapisu do registru dat
          fsm_state <= fsm_read_program_forward;


        when fsm_inc_data =>
          datareg_en <= "00"; -- konec zapisu do registru dat
          datareg_change <= "01"; -- data + 1
          fsm_state <= fsm_write_data;
			  
			


        when fsm_dec_data =>
          datareg_en <= "00"; -- konec zapisu do registru dat
          datareg_change <= "10"; -- data - 1
          fsm_state <= fsm_write_data;
        
        when fsm_write_data => 
			    datareg_change <= "00"; -- konec zmeny hodnoty dat
          DATA_EN <= '1'; -- zapnu RAM 
          DATA_ADDR <= iar_reg; -- adresa v pameti
          DATA_RDWR <= '1'; -- zapis
          DATA_WDATA <= data_reg; -- zapis inkrementovanych dat do RAM
			    fsm_wait <= fsm_wait + 1;
			    if(fsm_wait = "010") then -- 10 
				    DATA_RDWR <= '0';
				    fsm_wait <= "000";
				    fsm_state <= fsm_end; -- konec
			    end if;
			  
		
        when fsm_iar_inc =>
			 pcreg_change <= "00";
          DATA_EN <= '0';
          iarreg_change <= "01"; -- adresa v RAM + 1 
			 fsm_state <= fsm_end;


        when fsm_iar_dec =>
			    pcreg_change <= "00";
          DATA_EN <= '0';
          iarreg_change <= "10"; -- adresa v RAM - 1
			    fsm_state <= fsm_end;

        
        when fsm_end =>
			    pcreg_change <= "00";
          iarreg_change <= "00"; -- konec inkrementace adresy
          DATA_RDWR <= '0'; -- konec zapisu 
          DATA_EN <= '0'; -- vypnout RAM
          OUT_WE <= '0';
			    --fsm_state <= fsm_read_program_forward; 
          case(ireg_decode) is
            when read_stop =>

            when others =>
            fsm_state <= fsm_read_program_forward; 
          end case;

        when others =>

      end case;
    end if;
  end if;
end process FSM;
  -- FSM

end behavioral;