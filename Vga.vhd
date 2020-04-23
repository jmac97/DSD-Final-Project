----------------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Author:  Albert Fazakas adapted from Alec Wyen and Mihaita Nagy
--          Copyright 2014 Digilent, Inc.
----------------------------------------------------------------------------
-- 
-- Create Date:    13:01:51 02/15/2013 
-- Design Name: 
-- Module Name:    Vga - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--       This module represents the Vga controller that creates the HSYNC and VSYNC signals
--    for the VGA screen and formats the 4-bit R, G and B signals to display various items
--    on the screen:
--       - A moving colorbar in the background
--       - A Digilent - Analog Devices logo for the Nexys4 board, the RGB data is provided 
--    by the LogoDisplay component. The logo bitmap is stored in the BRAM_1 Block RAM in .ngc format.
--       - The FPGA temperature on a 0..80C scale. Temperature data is taken from the XADC
--    component in the Artix-7 FPGA, provided by the upper level FPGAMonitor component and the RGB data is
--    provided by the Inst_XadcTempDisplay instance of the TempDisplay component.
--       - The Nexys4 Onboard ADT7420 Temperature Sensor temperature on a 0..80C scale. 
--    Temperature data is provided by the upper level TempSensorCtl component and the RGB data is
--    provided by the Inst_Adt7420TempDisplay instance of the TempDisplay component.
--       - The Nexys4 Onboard ADXL362 Accelerometer Temperature Sensor temperature on a 0..80C scale. 
--    Temperature data is provided by the upper level AccelerometerCtl component and the RGB data is
--    provided by the Inst_Adxl362TempDisplay instance of the TempDisplay component.
--       - The R, G and B data which is also sent to the Nexys4 onboard RGB Leds LD16 and LD17. The 
--    incomming RGB Led data is taken from the upper level RgbLed component and the formatted RGB data is provided
--    by the RGBLedDisplay component.
--       - The audio signal coming from the Nexys4 Onboard ADMP421 Omnidirectional Microphone. The formatted
--    RGB data is provided by the MicDisplay component.
--       - The X and Y acceleration in a form of a moving box and the acceleration magnitude determined by 
--    the SQRT (X^2 + Y^2 + Z^2) formula. The acceleration and magnitude data is provided by the upper level 
--    AccelerometerCtl component and the formatted RGB data is provided by the AccelDisplay component.
--       - The mouse cursor on the top on all of the items. The USB mouse should be connected to the Nexys4 board before 
--    the FPGA is configured. The mouse cursor data is provided by the upper level MouseCtl component and the 
--    formatted RGB data for the mouse cursor shape is provided by the MouseDisplay component.
--       - An overlay that displayed the frames and text for the displayed items described above. The overlay data is
--    stored in the overlay_bram Block RAM in the .ngc format and the data is provided by the OverlayCtl component.
--       The Vga controller holds the synchronization signal generation, the moving colorbar generation and the main
--    multiplexers for the outgoing R, G and B signals. Also the 108 MHz pixel clock (pxl_clk) generator is instantiated
--    inside the Vga controller.
--       The current resolution is 1280X1024 pixels, however, other resolutions can also be selected by 
--    commenting/uncommenting the corresponding VGA resolution constants. In the case when a different resolution
--    is selected, the pixel clock generator output frequency also has to be updated accordingly.
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.math_real.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Vga is
    Port ( CLK_I : in  STD_LOGIC;
           -- VGA Output Signals
           VGA_HS_O : out  STD_LOGIC; -- HSYNC OUT
           VGA_VS_O : out  STD_LOGIC; -- VSYNC OUT
           VGA_RED_O    : out  STD_LOGIC_VECTOR (3 downto 0); -- Red signal going to the VGA interface
           VGA_GREEN_O  : out  STD_LOGIC_VECTOR (3 downto 0); -- Green signal going to the VGA interface
           VGA_BLUE_O   : out  STD_LOGIC_VECTOR (3 downto 0); -- Blue signal going to the VGA interface
           -- Input Signals
           -- Temperature data signals
           XADC_TEMP_VALUE_I     : in std_logic_vector (11 downto 0); -- FPGA Temperature data from the XADC
           ADT7420_TEMP_VALUE_I  : in std_logic_vector (12 downto 0); -- Temperature data from the Onboard Temperature Sensor
           ADXL362_TEMP_VALUE_I  : in std_logic_vector (11 downto 0)  -- Temperature Data from the Accelerometer
           );
end Vga;

architecture Behavioral of Vga is

-------------------------------------------------------------------------

-- Component Declarations

-------------------------------------------------------------------------


   -- To generate the 108 MHz Pixel Clock
   -- needed for a resolution of 1280*1024 pixels
--   COMPONENT PxlClkGen
--   PORT
--    (-- Clock in ports
--     CLK_IN1           : in std_logic;
--     -- Clock out ports
--     CLK_OUT1          : out std_logic;
--     -- Status and control signals
--     LOCKED            : out std_logic
--    );
--   END COMPONENT;
   
   -- Display the overlay
   COMPONENT OverlayCtl
	PORT(
		CLK_I : IN std_logic;
		VSYNC_I : IN std_logic;
		ACTIVE_I : IN std_logic;          
		OVERLAY_O : OUT std_logic
		);
	END COMPONENT;
   

   -- Display the FPGA, Temp Sensor and Accelerometer Temperature
   COMPONENT TempDisplay
   GENERIC(
           X_TMP_COL_WIDTH    : natural := 50;   -- = SZ_TH_WIDTH - width of a TMP column
           Y_TMP_COL_HEIGHT   : natural := 472;  -- = SZ_TH_HEIGHT - height of a TMP column
           X_TMP_H_LOC        : natural := 1050; -- X Location of the TMP Column
           Y_TMP_V_LOC        : natural := 80;   -- Y Location of the TMP Column
           INPUT_DATA_WIDTH   : natural := 13; -- Data width is 13 for XADC and 12 for Temperature Sensor and 
                                               -- Accelerometer Temperature Sensor
           TMP_TYPE           : string := "XADC"
           );
    PORT ( CLK_I     : in STD_LOGIC;
           TEMP_IN   : in STD_LOGIC_VECTOR (INPUT_DATA_WIDTH - 1 downto 0); -- Input Temperature Data
           H_COUNT_I : in  STD_LOGIC_VECTOR (11 downto 0);
           V_COUNT_I : in  STD_LOGIC_VECTOR (11 downto 0);
           -- Temperature Red, Green and Blue signals
           TEMP_R_OUT   : out STD_LOGIC_VECTOR (3 downto 0);
           TEMP_G_OUT   : out STD_LOGIC_VECTOR (3 downto 0);
           TEMP_B_OUT   : out STD_LOGIC_VECTOR (3 downto 0)
          );
   END COMPONENT;

------------------------------------------------------------

-- Constants for various VGA Resolutions

-------------------------------------------------------------

--***640x480@60Hz***--  
--constant FRAME_WIDTH : natural := 640;
--constant FRAME_HEIGHT : natural := 480;

--constant H_FP : natural := 16; --H front porch width (pixels)
--constant H_PW : natural := 96; --H sync pulse width (pixels)
--constant H_MAX : natural := 800; --H total period (pixels)
--
--constant V_FP : natural := 10; --V front porch width (lines)
--constant V_PW : natural := 2; --V sync pulse width (lines)
--constant V_MAX : natural := 525; --V total period (lines)

--constant H_POL : std_logic := '0';
--constant V_POL : std_logic := '0';

--***800x600@60Hz***--
--constant FRAME_WIDTH : natural := 800;
--constant FRAME_HEIGHT : natural := 600;
--
--constant H_FP : natural := 40; --H front porch width (pixels)
--constant H_PW : natural := 128; --H sync pulse width (pixels)
--constant H_MAX : natural := 1056; --H total period (pixels)
--
--constant V_FP : natural := 1; --V front porch width (lines)
--constant V_PW : natural := 4; --V sync pulse width (lines)
--constant V_MAX : natural := 628; --V total period (lines)
--
--constant H_POL : std_logic := '1';
--constant V_POL : std_logic := '1';

--***1280x1024@60Hz***--
constant FRAME_WIDTH : natural := 1280;
constant FRAME_HEIGHT : natural := 1024;

constant H_FP : natural := 48; --H front porch width (pixels)
constant H_PW : natural := 112; --H sync pulse width (pixels)
constant H_MAX : natural := 1688; --H total period (pixels)

constant V_FP : natural := 1; --V front porch width (lines)
constant V_PW : natural := 3; --V sync pulse width (lines)
constant V_MAX : natural := 1066; --V total period (lines)

constant H_POL : std_logic := '1';
constant V_POL : std_logic := '1';

--***1920x1080@60Hz***--
--constant FRAME_WIDTH : natural := 1920;
--constant FRAME_HEIGHT : natural := 1080;
--
--constant H_FP : natural := 88; --H front porch width (pixels)
--constant H_PW : natural := 44; --H sync pulse width (pixels)
--constant H_MAX : natural := 2200; --H total period (pixels)
--
--constant V_FP : natural := 4; --V front porch width (lines)
--constant V_PW : natural := 5; --V sync pulse width (lines)
--constant V_MAX : natural := 1125; --V total period (lines)
--
--constant H_POL : std_logic := '1';
--constant V_POL : std_logic := '1';



------------------------------------------------------------------------------

-- Constants for setting the temperature display columns size and coordinates

-------------------------------------------------------------------------------
constant SZ_TEMP_WIDTH		: natural := 50; -- Width of a Temp Column
constant SZ_TEMP_HEIGHT		: natural := 472; -- Height of a Temp column

-- Starting Horizontal and Vertical locations of the three temperature columns
-- FPGA Temperature
constant FRM_XADC_TEMP_H_LOC	   : natural := 1050;
constant FRM_XADC_TEMP_V_LOC	   : natural := 80;
-- ADT7420 Temperature Sensor
constant FRM_ADT7420_TEMP_H_LOC	: natural := 1125;
constant FRM_ADT7420_TEMP_V_LOC	: natural := 80;
-- ADXL362 Accelerometer temperature
constant FRM_ADXL362_TEMP_H_LOC  : natural := 1200;
constant FRM_ADXL362_TEMP_V_LOC	: natural := 80;

-- Limits of the Temperature Column Frames
-- FPGA
constant XADC_TEMP_RIGHT	   : natural := FRM_XADC_TEMP_H_LOC + SZ_TEMP_WIDTH + 1;
constant XADC_TEMP_LEFT		   : natural := FRM_XADC_TEMP_H_LOC - 1;
constant XADC_TEMP_TOP		   : natural := FRM_XADC_TEMP_V_LOC - 1;
constant XADC_TEMP_BOTTOM	   : natural := FRM_XADC_TEMP_V_LOC + SZ_TEMP_HEIGHT + 1;
-- ADT7420
constant ADT7420_TEMP_RIGHT	: natural := FRM_ADT7420_TEMP_H_LOC + SZ_TEMP_WIDTH + 1;
constant ADT7420_TEMP_LEFT		: natural := FRM_ADT7420_TEMP_H_LOC - 1;
constant ADT7420_TEMP_TOP		: natural := FRM_ADT7420_TEMP_V_LOC - 1;
constant ADT7420_TEMP_BOTTOM	: natural := FRM_ADT7420_TEMP_V_LOC + SZ_TEMP_HEIGHT + 1;
--ADXL362
constant ADXL362_TEMP_RIGHT	: natural := FRM_ADXL362_TEMP_H_LOC + SZ_TEMP_WIDTH + 1;
constant ADXL362_TEMP_LEFT		: natural := FRM_ADXL362_TEMP_H_LOC - 1;
constant ADXL362_TEMP_TOP		: natural := FRM_ADXL362_TEMP_V_LOC - 1;
constant ADXL362_TEMP_BOTTOM	: natural := FRM_ADXL362_TEMP_V_LOC + SZ_TEMP_HEIGHT + 1;

----------------------------------------------------------------------------------------------------

-- Constants for setting size and location for TBOX - the white box holding the temperature columns

-----------------------------------------------------------------------------------------------------
constant SZ_TBOX_WIDTH 		: natural := 278; -- TBOX width
constant SZ_TBOX_HEIGHT		: natural := 553; -- TBOX height

constant FRM_TBOX_H_LOC		: natural := 985; -- TBOX starting horizontal location
constant FRM_TBOX_V_LOC		: natural := 40; -- TBOX starting vertical location
-- TBOX frame limits
constant TBOX_LEFT			: natural := FRM_TBOX_H_LOC - 1;
constant TBOX_RIGHT			: natural := FRM_TBOX_H_LOC + SZ_TBOX_WIDTH + 1;
constant TBOX_TOP				: natural := FRM_TBOX_V_LOC - 1;
constant TBOX_BOTTOM			: natural := FRM_TBOX_V_LOC + SZ_TBOX_HEIGHT + 1;

-------------------------------------------------------------------------

-- Signal Declarations

-------------------------------------------------------------------------


-------------------------------------------------------------------------

-- VGA Controller specific signals: Counters, Sync, R, G, B

-------------------------------------------------------------------------
-- Pixel clock, in this case 108 MHz
signal pxl_clk : std_logic;
-- The active signal is used to signal the active region of the screen (when not blank)
signal active  : std_logic;

-- Horizontal and Vertical counters
signal h_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');
signal v_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');

-- Pipe Horizontal and Vertical Counters
signal h_cntr_reg_dly   : std_logic_vector(11 downto 0) := (others => '0');
signal v_cntr_reg_dly   : std_logic_vector(11 downto 0) := (others => '0');

-- Horizontal and Vertical Sync
signal h_sync_reg : std_logic := not(H_POL);
signal v_sync_reg : std_logic := not(V_POL);
-- Pipe Horizontal and Vertical Sync
signal h_sync_reg_dly : std_logic := not(H_POL);
signal v_sync_reg_dly : std_logic :=  not(V_POL);

-- VGA R, G and B signals coming from the main multiplexers
signal vga_red_cmb   : std_logic_vector(3 downto 0);
signal vga_green_cmb : std_logic_vector(3 downto 0);
signal vga_blue_cmb  : std_logic_vector(3 downto 0);
--The main VGA R, G and B signals, validated by active
signal vga_red    : std_logic_vector(3 downto 0);
signal vga_green  : std_logic_vector(3 downto 0);
signal vga_blue   : std_logic_vector(3 downto 0);
-- Register VGA R, G and B signals
signal vga_red_reg   : std_logic_vector(3 downto 0) := (others =>'0');
signal vga_green_reg : std_logic_vector(3 downto 0) := (others =>'0');
signal vga_blue_reg  : std_logic_vector(3 downto 0) := (others =>'0');

-------------------------------------------------------------------------

-- Signals for registering the inputs

-------------------------------------------------------------------------
signal XADC_TEMP_VALUE_I_REG     : std_logic_vector (11 downto 0);
signal ADT7420_TEMP_VALUE_I_REG  : std_logic_vector (12 downto 0);
signal ADXL362_TEMP_VALUE_I_REG  : std_logic_vector (11 downto 0);

-----------------------------------------------------------
-- Signals for generating the background (moving colorbar)
-----------------------------------------------------------
signal cntDyn				: integer range 0 to 2**28-1; -- counter for generating the colorbar
signal intHcnt				: integer range 0 to H_MAX - 1;
signal intVcnt				: integer range 0 to V_MAX - 1;
-- Colorbar red, greeen and blue signals
signal bg_red 				: std_logic_vector(3 downto 0);
signal bg_blue 			: std_logic_vector(3 downto 0);
signal bg_green 			: std_logic_vector(3 downto 0);
-- Pipe the colorbar red, green and blue signals
signal bg_red_dly			: std_logic_vector(3 downto 0) := (others => '0');
signal bg_green_dly		: std_logic_vector(3 downto 0) := (others => '0');
signal bg_blue_dly		: std_logic_vector(3 downto 0) := (others => '0');


-------------------------------------------------------------------------

-- Interconnection signals for the displaying components

-------------------------------------------------------------------------

-- FPGA Temperature Display Signals
signal xadc_temp_red    : std_logic_vector (3 downto 0);
signal xadc_temp_green  : std_logic_vector (3 downto 0);
signal xadc_temp_blue   : std_logic_vector (3 downto 0);

-- ADT740 Temperature Sensor Display Signals
signal adt7420_temp_red    : std_logic_vector (3 downto 0);
signal adt7420_temp_green  : std_logic_vector (3 downto 0);
signal adt7420_temp_blue   : std_logic_vector (3 downto 0);

-- TBOX (frame holding the temperature columns) display signals
signal tbox_red			: std_logic_vector(3 downto 0);
signal tbox_blue			: std_logic_vector(3 downto 0);
signal tbox_green			: std_logic_vector(3 downto 0);

-- Overlay display signal
signal overlay_en : std_logic;

---------------------------------------------------------------------------------

-- Pipe all of the interconnection signals coming from the displaying components

---------------------------------------------------------------------------------

-- Registered FPGA Temperature Display Signals
signal xadc_temp_red_dly    : std_logic_vector (3 downto 0);
signal xadc_temp_green_dly  : std_logic_vector (3 downto 0);
signal xadc_temp_blue_dly   : std_logic_vector (3 downto 0);

-- Registered ADT740 Temperature Sensor Display Signals
signal adt7420_temp_red_dly    : std_logic_vector (3 downto 0);
signal adt7420_temp_green_dly  : std_logic_vector (3 downto 0);
signal adt7420_temp_blue_dly   : std_logic_vector (3 downto 0);

-- TBOX (frame holding the temperature columns) color is white,
-- therefore TBOX signals will not be registered again

-- Registered Overlay display signal
signal overlay_en_dly : std_logic; 

begin
  
  pxl_clk <= CLK_I; 
------------------------------------

-- Generate the 108 MHz pixel clock 

------------------------------------
--   Inst_PxlClkGen: PxlClkGen
--   port map
--    (-- Clock in ports
--     CLK_IN1   => CLK_I,
--     -- Clock out ports
--     CLK_OUT1  => pxl_clk,
--     -- Status and control signals
--     LOCKED   => open
--    );

---------------------------------------------------------------

-- Generate Horizontal, Vertical counters and the Sync signals

---------------------------------------------------------------
  -- Horizontal counter
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (h_cntr_reg = (H_MAX - 1)) then
        h_cntr_reg <= (others =>'0');
      else
        h_cntr_reg <= h_cntr_reg + 1;
      end if;
    end if;
  end process;
  -- Vertical counter
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if ((h_cntr_reg = (H_MAX - 1)) and (v_cntr_reg = (V_MAX - 1))) then
        v_cntr_reg <= (others =>'0');
      elsif (h_cntr_reg = (H_MAX - 1)) then
        v_cntr_reg <= v_cntr_reg + 1;
      end if;
    end if;
  end process;
  -- Horizontal sync
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (h_cntr_reg >= (H_FP + FRAME_WIDTH - 1)) and (h_cntr_reg < (H_FP + FRAME_WIDTH + H_PW - 1)) then
        h_sync_reg <= H_POL;
      else
        h_sync_reg <= not(H_POL);
      end if;
    end if;
  end process;
  -- Vertical sync
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (v_cntr_reg >= (V_FP + FRAME_HEIGHT - 1)) and (v_cntr_reg < (V_FP + FRAME_HEIGHT + V_PW - 1)) then
        v_sync_reg <= V_POL;
      else
        v_sync_reg <= not(V_POL);
      end if;
    end if;
  end process;
  
--------------------

-- The active 

--------------------  
  -- active signal
  active <= '1' when h_cntr_reg_dly < FRAME_WIDTH and v_cntr_reg_dly < FRAME_HEIGHT
            else '0';

--------------------

-- Register Inputs

--------------------
register_inputs: process (pxl_clk, v_sync_reg)
  begin
    if (rising_edge(pxl_clk)) then
      if v_sync_reg = V_POL then -- All of the signals, except the incoming microphone data 
                                 -- have lover frequencies than the vertical refresh rate,
                                 -- therefore will be registered in the blanking area
         
         XADC_TEMP_VALUE_I_REG      <= XADC_TEMP_VALUE_I;
         ADT7420_TEMP_VALUE_I_REG   <= ADT7420_TEMP_VALUE_I;
         ADXL362_TEMP_VALUE_I_REG   <= ADXL362_TEMP_VALUE_I;
   
      end if;   
    end if;
end process register_inputs;

   
 --------------------------------

-- Temperature display instances

---------------------------------
 ----------------------------------------------------
-- FPGA Temperature - from the XADC temperature data
-----------------------------------------------------
   Inst_XadcTempDisplay: TempDisplay
   GENERIC MAP(
           X_TMP_COL_WIDTH    => SZ_TEMP_WIDTH, -- width of the TEMP column
           Y_TMP_COL_HEIGHT   => SZ_TEMP_HEIGHT,-- height of the TEMP column
           X_TMP_H_LOC        => FRM_XADC_TEMP_H_LOC, -- X Location of the FPGA TEMP Column
           Y_TMP_V_LOC        => FRM_XADC_TEMP_V_LOC, -- Y Location of the FPGA TEMP Column
           INPUT_DATA_WIDTH   => 12,
           TMP_TYPE           => "XADC"
           )
    PORT MAP ( 
           CLK_I        => pxl_clk,
           TEMP_IN      => XADC_TEMP_VALUE_I_REG,
           H_COUNT_I    => h_cntr_reg,
           V_COUNT_I    => v_cntr_reg,
           -- Temperature Red, Green and Blue signals
           TEMP_R_OUT   => xadc_temp_red,
           TEMP_G_OUT   => xadc_temp_green,
           TEMP_B_OUT   => xadc_temp_blue
          );
          
-------------------------------------------------
-- ADT740 onboard temperature sensor temperature
-------------------------------------------------
   Inst_Adt7420TempDisplay: TempDisplay
   GENERIC MAP (
           X_TMP_COL_WIDTH    => SZ_TEMP_WIDTH, -- width of the TEMP column
           Y_TMP_COL_HEIGHT   => SZ_TEMP_HEIGHT,-- height of the TEMP column
           X_TMP_H_LOC        => FRM_ADT7420_TEMP_H_LOC, -- X Location of the Temp Sensor Column
           Y_TMP_V_LOC        => FRM_ADT7420_TEMP_V_LOC, -- Y Location of the Temp Sensor Column
           INPUT_DATA_WIDTH   => 13,
           TMP_TYPE           => "TEMP_ACC"
           )
    PORT MAP (
           CLK_I        => pxl_clk,
           TEMP_IN      => ADT7420_TEMP_VALUE_I_REG,
           H_COUNT_I    => h_cntr_reg,
           V_COUNT_I    => v_cntr_reg,
           -- Temperature Red, Green and Blue signals
           TEMP_R_OUT   => adt7420_temp_red,
           TEMP_G_OUT   => adt7420_temp_green,
           TEMP_B_OUT   => adt7420_temp_blue
          );
----------------------------------------------------
-- ADXL362 onboard accelerometer temperature sensor
----------------------------------------------------
    Inst_Adxl362TempDisplay: TempDisplay
   GENERIC MAP(
           X_TMP_COL_WIDTH    => SZ_TEMP_WIDTH, -- width of the TEMP column
           Y_TMP_COL_HEIGHT   => SZ_TEMP_HEIGHT,-- height of the TEMP column
           X_TMP_H_LOC        => FRM_ADXL362_TEMP_H_LOC, -- X Location of the ACC Temp Column
           Y_TMP_V_LOC        => FRM_ADXL362_TEMP_V_LOC, -- Y Location of the Acc Temp Column
           INPUT_DATA_WIDTH   => 12,
           TMP_TYPE           => "TEMP_ACC"
           )
    PORT MAP (
           CLK_I        => pxl_clk,
           TEMP_IN      => ADXL362_TEMP_VALUE_I_REG,
           H_COUNT_I    => h_cntr_reg,
           V_COUNT_I    => v_cntr_reg,
           -- Temperature Red, Green and Blue signals
           TEMP_R_OUT   => adxl362_temp_red,
           TEMP_G_OUT   => adxl362_temp_green,
           TEMP_B_OUT   => adxl362_temp_blue
          ); 
          
   
----------------------------------

-- Overlay display instance

----------------------------------
    	Inst_OverlayCtrl: OverlayCtl 
      PORT MAP
      (
		CLK_I       => pxl_clk,
		VSYNC_I     => v_sync_reg,
		ACTIVE_I    => active,
		OVERLAY_O   => overlay_en
      );
  
  
---------------------------------------

-- Generate moving colorbar background

---------------------------------------

	process(pxl_clk)
	begin
		if(rising_edge(pxl_clk)) then
			cntdyn <= cntdyn + 1;
		end if;
	end process;
   
  	intHcnt <= conv_integer(h_cntr_reg);
	intVcnt <= conv_integer(v_cntr_reg);
	
	bg_red <= conv_std_logic_vector((-intvcnt - inthcnt - cntDyn/2**20),8)(7 downto 4);
	bg_green <= conv_std_logic_vector((inthcnt - cntDyn/2**20),8)(7 downto 4);
	bg_blue <= conv_std_logic_vector((intvcnt - cntDyn/2**20),8)(7 downto 4);
   

----------------------------------------------------------------------
-- TBOX (the frame that holds the temperature columns) color is white
----------------------------------------------------------------------
   tbox_red    <= X"F";
   tbox_blue   <= X"F";
   tbox_green  <= X"F";


---------------------------------------------------------------------------------------------------

-- Register Outputs coming from the displaying components and the horizontal and vertical counters

---------------------------------------------------------------------------------------------------
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
   
      
      xadc_temp_red_dly    <= xadc_temp_red;
      xadc_temp_green_dly  <= xadc_temp_green;
      xadc_temp_blue_dly   <= xadc_temp_blue;

      adt7420_temp_red_dly    <= adt7420_temp_red;
      adt7420_temp_green_dly  <= adt7420_temp_green;
      adt7420_temp_blue_dly   <= adt7420_temp_blue;

      adxl362_temp_red_dly    <= adxl362_temp_red;
      adxl362_temp_green_dly  <= adxl362_temp_green;
      adxl362_temp_blue_dly   <= adxl362_temp_blue;


      acl_red_dly			<= acl_red;
		acl_green_dly		<= acl_green;
		acl_blue_dly		<= acl_blue;
      
      bg_red_dly			<= bg_red;
		bg_green_dly		<= bg_green;
		bg_blue_dly			<= bg_blue;


      overlay_en_dly <= overlay_en;
      
      h_cntr_reg_dly <= h_cntr_reg;
		v_cntr_reg_dly <= v_cntr_reg;
      
      
    end if;
  end process;


-------------------------------------------------------------

-- Main Multiplexers for the VGA Red, Green and Blue signals

-------------------------------------------------------------
----------
-- Red
----------

  vga_red <=   
               -- Temperature display
               xadc_temp_red_dly when h_cntr_reg_dly > XADC_TEMP_LEFT and h_cntr_reg_dly < XADC_TEMP_RIGHT 
                                  and v_cntr_reg_dly > XADC_TEMP_TOP and v_cntr_reg_dly < XADC_TEMP_BOTTOM
               else
               adt7420_temp_red_dly when h_cntr_reg_dly > ADT7420_TEMP_LEFT and h_cntr_reg_dly < ADT7420_TEMP_RIGHT 
                                     and v_cntr_reg_dly > ADT7420_TEMP_TOP and v_cntr_reg_dly < ADT7420_TEMP_BOTTOM
               else
               adxl362_temp_red_dly when h_cntr_reg_dly > ADXL362_TEMP_LEFT and h_cntr_reg_dly < ADXL362_TEMP_RIGHT 
                                     and v_cntr_reg_dly > ADXL362_TEMP_TOP and v_cntr_reg_dly < ADXL362_TEMP_BOTTOM
               else
               -- TBOX display
     			   tbox_red when h_cntr_reg_dly > TBOX_LEFT and h_cntr_reg_dly < TBOX_RIGHT 
                         and v_cntr_reg_dly < TBOX_BOTTOM and v_cntr_reg_dly > TBOX_TOP
               else
               -- Colorbar will be on the backround
               bg_red_dly;
                
-----------
-- Green
-----------

  vga_green <= 
               -- Temperature display
               xadc_temp_green_dly when h_cntr_reg_dly > XADC_TEMP_LEFT and h_cntr_reg_dly < XADC_TEMP_RIGHT 
                                    and v_cntr_reg_dly > XADC_TEMP_TOP and v_cntr_reg_dly < XADC_TEMP_BOTTOM
               else
               adt7420_temp_green_dly when h_cntr_reg_dly > ADT7420_TEMP_LEFT and h_cntr_reg_dly < ADT7420_TEMP_RIGHT 
                                       and v_cntr_reg_dly > ADT7420_TEMP_TOP and v_cntr_reg_dly < ADT7420_TEMP_BOTTOM
               else
               adxl362_temp_green_dly when h_cntr_reg_dly > ADXL362_TEMP_LEFT and h_cntr_reg_dly < ADXL362_TEMP_RIGHT 
                                       and v_cntr_reg_dly > ADXL362_TEMP_TOP and v_cntr_reg_dly < ADXL362_TEMP_BOTTOM
               else
               -- TBOX display
     			   tbox_green when h_cntr_reg_dly > TBOX_LEFT and h_cntr_reg_dly < TBOX_RIGHT 
                           and v_cntr_reg_dly < TBOX_BOTTOM and v_cntr_reg_dly > TBOX_TOP
               else
               -- Colorbar will be on the backround
               bg_green_dly;

-----------
-- Blue
-----------

  vga_blue <= 
               -- Temperature display
               xadc_temp_blue_dly when h_cntr_reg_dly > XADC_TEMP_LEFT and h_cntr_reg_dly < XADC_TEMP_RIGHT 
                                   and v_cntr_reg_dly > XADC_TEMP_TOP and v_cntr_reg_dly < XADC_TEMP_BOTTOM
               else
               adt7420_temp_blue_dly when h_cntr_reg_dly > ADT7420_TEMP_LEFT and h_cntr_reg_dly < ADT7420_TEMP_RIGHT 
                                      and v_cntr_reg_dly > ADT7420_TEMP_TOP and v_cntr_reg_dly < ADT7420_TEMP_BOTTOM
               else
               adxl362_temp_blue_dly when h_cntr_reg_dly > ADXL362_TEMP_LEFT and h_cntr_reg_dly < ADXL362_TEMP_RIGHT 
                                      and v_cntr_reg_dly > ADXL362_TEMP_TOP and v_cntr_reg_dly < ADXL362_TEMP_BOTTOM
               else
               -- TBOX display
     			   tbox_blue when h_cntr_reg_dly > TBOX_LEFT and h_cntr_reg_dly < TBOX_RIGHT 
                          and v_cntr_reg_dly < TBOX_BOTTOM and v_cntr_reg_dly > TBOX_TOP
               else
               -- Colorbar will be on the backround
               bg_blue_dly;
                

------------------------------------------------------------
-- Turn Off VGA RBG Signals if outside of the active screen
-- Make a 4-bit AND logic with the R, G and B signals
------------------------------------------------------------
 vga_red_cmb <= (active & active & active & active) and vga_red;
 vga_green_cmb <= (active & active & active & active) and vga_green;
 vga_blue_cmb <= (active & active & active & active) and vga_blue;
 

 -- Register Outputs
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then

      v_sync_reg_dly <= v_sync_reg;
      h_sync_reg_dly <= h_sync_reg;
      vga_red_reg    <= vga_red_cmb;
      vga_green_reg  <= vga_green_cmb;
      vga_blue_reg   <= vga_blue_cmb;      
    end if;
  end process;

  -- Assign outputs
  VGA_HS_O     <= h_sync_reg_dly;
  VGA_VS_O     <= v_sync_reg_dly;
  VGA_RED_O    <= vga_red_reg;
  VGA_GREEN_O  <= vga_green_reg;
  VGA_BLUE_O   <= vga_blue_reg;

end Behavioral;
