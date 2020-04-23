----------------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Author:  Albert Fazakas adapted from Sam Bobrowicz and Mihaita Nagy
--          Copyright 2014 Digilent, Inc.
----------------------------------------------------------------------------

-- Design Name:    Nexys4 DDR User Demo
-- Module Name:    Nexys4DdrUserDemo - Behavioral 
-- Project Name: 
-- Target Devices: Nexys4 DDR Development Board, containing a XC7a100t-1 csg324 device
-- Tool versions: 
-- Description: 
-- This module represents the top - level design of the Nexys4 DDR User Demo.
-- The project connects to the VGA display in a 1280*1024 resolution and displays various
-- items on the screen:
--    - a Digilent / Analog Devices logo
--
--    - a mouse cursor, if an Usb mouse is connected to the board when the project is started
--
--    - the audio signal from the onboard ADMP421 Omnidirectional Microphone

--    - a small square representing the X and Y acceleration data from the ADXL362 onboard Accelerometer.
--      The square moves according the Nexys4 board position. Note that the X and Y axes 
--      on the board are exchanged due to the accelerometer layout on the Nexys4 board.
--      The accelerometer display also displays the acceleration magnitude, calculated as
--      SQRT( X^2 + Y^2 +Z^2), where X, Y and Z represent the acceleration value on the respective axes
--
--    - The FPGA temperature, the onboard ADT7420 temperature sensor temperature value and the accelerometer
--      temperature value
--
--    - The value of the R, G and B components sent to the RGB Leds LD16 and LD17
--
-- Other features:
--    - The 16 Switches (SW0..SW15) are connected to LD0..LD15 except when audio recording is done
--
--    - Pressing BTNL, BTNC and BTNR will toggle between Red, Green and Blue colors on LD16 and LD17
--      Color sweeping returns when BTND is pressed. BTND also togles between LD16, LD17, none or both
--
--    - Pressing BTNU will start audio recording for about 5S, then the audio data will be played back
--      on the Audio output. While recording, LD15..LD0 will show a progressbar moving to left, while
--      playing back, LD15..LD0 will show a progressbar moving to right
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Nexys4DdrUserDemo is
   port(
      clk_i          : in  std_logic;
      rstn_i         : in  std_logic;
      -- VGA display
      vga_hs_o       : out std_logic;
      vga_vs_o       : out std_logic;
      vga_red_o      : out std_logic_vector(3 downto 0);
      vga_blue_o     : out std_logic_vector(3 downto 0);
      vga_green_o    : out std_logic_vector(3 downto 0);
		-- Temperature sensor
		tmp_scl        : inout std_logic;
		tmp_sda        : inout std_logic;
--		tmp_int        : in std_logic; -- Not used in this project
--		tmp_ct         : in std_logic; -- Not used in this project
     
      -- Debug output signals
--      SCLK_DBG       : out STD_LOGIC;
--      MOSI_DBG       : out STD_LOGIC;
--      MISO_DBG       : out STD_LOGIC;
--      SS_DBG         : out STD_LOGIC;
      
--      PS2C_DBG       : out std_logic;
--      PS2D_DBG       : out std_logic;
      
      -- DDR2 interface signals
      ddr2_addr      : out   std_logic_vector(12 downto 0);
      ddr2_ba        : out   std_logic_vector(2 downto 0);
      ddr2_ras_n     : out   std_logic;
      ddr2_cas_n     : out   std_logic;
      ddr2_we_n      : out   std_logic;
      ddr2_ck_p      : out   std_logic_vector(0 downto 0);
      ddr2_ck_n      : out   std_logic_vector(0 downto 0);
      ddr2_cke       : out   std_logic_vector(0 downto 0);
      ddr2_cs_n      : out   std_logic_vector(0 downto 0);
      ddr2_dm        : out   std_logic_vector(1 downto 0);
      ddr2_odt       : out   std_logic_vector(0 downto 0);
      ddr2_dq        : inout std_logic_vector(15 downto 0);
      ddr2_dqs_p     : inout std_logic_vector(1 downto 0);
      ddr2_dqs_n     : inout std_logic_vector(1 downto 0)

   );
end Nexys4DdrUserDemo;

architecture Behavioral of Nexys4DdrUserDemo is

----------------------------------------------------------------------------------
-- Component Declarations
----------------------------------------------------------------------------------  

-- 200 MHz Clock Generator
component ClkGen
port
 (-- Clock in ports
  clk_100MHz_i           : in     std_logic;
  -- Clock out ports
  clk_100MHz_o          : out    std_logic;
  clk_200MHz_o          : out    std_logic;
  -- Status and control signals
  reset_i             : in     std_logic;
  locked_o            : out    std_logic
 );
end component;

component TempSensorCtl is
	Generic (CLOCKFREQ : natural := 100); -- input CLK frequency in MHz
	Port (
		TMP_SCL : inout STD_LOGIC;
		TMP_SDA : inout STD_LOGIC;
      -- The Interrupt and Critical Temperature Signals
      -- from the ADT7420 Temperature Sensor are not used in this design
--		TMP_INT : in STD_LOGIC;
--		TMP_CT : in STD_LOGIC;		
		TEMP_O : out STD_LOGIC_VECTOR(12 downto 0); --12-bit two's complement temperature with sign bit
		RDY_O : out STD_LOGIC;	--'1' when there is a valid temperature reading on TEMP_O
		ERR_O : out STD_LOGIC; --'1' if communication error
		CLK_I : in STD_LOGIC;
		SRST_I : in STD_LOGIC
	);
end component;

COMPONENT Vga is
PORT( 
   clk_i          : in  std_logic;
   vga_hs_o       : out std_logic;
   vga_vs_o       : out std_logic;
   vga_red_o      : out std_logic_vector(3 downto 0);
   vga_blue_o     : out std_logic_vector(3 downto 0);
   vga_green_o    : out std_logic_vector(3 downto 0);
   RGB_LED_RED    : in STD_LOGIC_VECTOR (7 downto 0);
   RGB_LED_GREEN  : in STD_LOGIC_VECTOR (7 downto 0);
   RGB_LED_BLUE   : in STD_LOGIC_VECTOR (7 downto 0);
   ACCEL_RADIUS   : in  STD_LOGIC_VECTOR (11 downto 0);
   LEVEL_THRESH   : in  STD_LOGIC_VECTOR (11 downto 0);
	ACL_X_IN       : in  STD_LOGIC_VECTOR (8 downto 0);
   ACL_Y_IN       : in  STD_LOGIC_VECTOR (8 downto 0);
   ACL_MAG_IN     : in  STD_LOGIC_VECTOR (11 downto 0);
   MIC_M_DATA_I   : IN STD_LOGIC;
   MIC_M_CLK_RISING  : IN STD_LOGIC;
   MOUSE_X_POS    :  in std_logic_vector (11 downto 0);
   MOUSE_Y_POS    :  in std_logic_vector (11 downto 0);
   XADC_TEMP_VALUE_I : in std_logic_vector (11 downto 0);
   ADT7420_TEMP_VALUE_I : in std_logic_vector (12 downto 0);
   ADXL362_TEMP_VALUE_I : in std_logic_vector (11 downto 0)
   );
END COMPONENT;

----------------------------------------------------------------------------------
-- Signal Declarations
----------------------------------------------------------------------------------  
-- Inverted input reset signal
signal rst        : std_logic;
-- Reset signal conditioned by the PLL lock
signal reset      : std_logic;
signal resetn     : std_logic;
signal locked     : std_logic;

-- 100 MHz buffered clock signal
signal clk_100MHz_buf : std_logic;
-- 200 MHz buffered clock signal
signal clk_200MHz_buf : std_logic;

-- ADT7420 Temperature Sensor raw Data Signal
signal tempValue : std_logic_vector(12 downto 0);
signal tempRdy, tempErr : std_logic;

-- XADC Temperature Sensor raw Data signal
signal fpgaTempValue : std_logic_vector(11 downto 0);



begin
   
   
----------------------------------------------------------------------------------
-- 200MHz Clock Generator
----------------------------------------------------------------------------------
   Inst_ClkGen: ClkGen
   port map (
      clk_100MHz_i   => clk_i,
      clk_100MHz_o   => clk_100MHz_buf,
      clk_200MHz_o   => clk_200MHz_buf,
      reset_i        => rst,
      locked_o       => locked
      );


  
----------------------------------------------------------------------------------
-- FPGA Temperature Monitor
----------------------------------------------------------------------------------
	Inst_FPGAMonitor: entity work.FPGAMonitor PORT MAP(
		CLK_I          => clk_100MHz_buf,
		RST_I          => reset,
		TEMP_O         => fpgaTempValue
	);

----------------------------------------------------------------------------------
-- Temperature Sensor Controller
----------------------------------------------------------------------------------
	Inst_TempSensorCtl: TempSensorCtl
	GENERIC MAP (CLOCKFREQ => 100)
	PORT MAP(
		TMP_SCL        => TMP_SCL,
		TMP_SDA        => TMP_SDA,
--		TMP_INT        => TMP_INT,
--		TMP_CT         => TMP_CT,		
		TEMP_O         => tempValue,
		RDY_O          => tempRdy,
		ERR_O          => tempErr,
		
		CLK_I          => clk_100MHz_buf,
		SRST_I         => reset
	);

----------------------------------------------------------------------------------
-- VGA Controller
----------------------------------------------------------------------------------
   Inst_VGA: Vga
   port map(
      clk_i          => clk_100MHz_buf,
      vga_hs_o       => vga_hs_o,
      vga_vs_o       => vga_vs_o,
      vga_red_o      => vga_red_o,
      vga_blue_o     => vga_blue_o,
      vga_green_o    => vga_green_o,
      XADC_TEMP_VALUE_I => fpgaTempValue,
      ADT7420_TEMP_VALUE_I => tempValue,
      ADXL362_TEMP_VALUE_I => ACCEL_TMP
      );  
end Behavioral;
