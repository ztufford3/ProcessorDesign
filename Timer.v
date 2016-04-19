module Timer (
	input wire [31:0] ABUS,
	input wire [31:0] DBUSI,
	input wire WE,CLK,
	output wire [31:0] DBUSO/*,LOCK,INIT,
	output wire INTR*/ );
	
	parameter BITS = 32;
	parameter TCNTADDR = 32'hFFFFF100;
	parameter TLIMADDR = 32'hFFFFF104;
	parameter TCTLADDR = 32'hFFFFF108;
	
	parameter FREQ = 32'd50;
   parameter MILLS = FREQ*32'd10000;
	//parameter MILLS = 32'd2;
	
	wire selCnt = (ABUS == TCNTADDR);
	wire selLim = (ABUS == TLIMADDR);
	wire selCtl = (ABUS == TCTLADDR);
	wire wrCnt = selCnt && WE;
	wire rdCnt = selCnt && (!WE);
	wire wrLim = selLim && WE;
	wire rdLim = selLim && (!WE);
	wire wrCtl = selCtl && WE && !(DBUSI[1] | DBUSI[0]);
	wire rdCtl = selCtl && (!WE);
	
	//Timer limit register
	reg [31:0] TCNT=32'b0;
	//Timer control register
	reg [4:0] TCTL=4'b0;
	//Timer count register
	reg [31:0] TLIM=32'b0;
	reg [31:0] count;
	//will change the size of TCTL as needed
	//count of clock cycles to track 1 millisecond
	wire checkMilsec = ((count%(MILLS-1))==0);
	wire limEn = (TLIM!=0);
	wire limHit = TCNT==(TLIM-1);
	
	//basically, if it hasn't been a millisecond, feed back in same values. If TCNT is within
	//the TLIM-1 range, increment it. If TLIM is nonzero and TCNT is out of bounds, the read
	//bit is set and TCNT goes back to 0. If the read bit was already set, the overflow bit
	//gets set.
	always @(posedge CLK) begin
		count<=count+32'd1;
		TCTL[1]<=(wrCtl)?1'b0:
					(limHit&limEn&TCTL[0])?1'b1:
					TCTL[1];
		TCTL[0]<=(wrCtl)?1'b0:
					(limHit&limEn)?1'b1:
					TCTL[0];
		
		TCNT<=	(wrCnt)?DBUSI:
					(limHit&&limEn)?32'b0:
					checkMilsec?(TCNT+32'd1):
					TCNT;
		TLIM<=(wrLim)?DBUSI:TLIM;
		//TCTL<=(wrCtl)?DBUSI:TCTL;
	end
	
	wire [31:0] DBUS_CNT = (rdCnt)?TCNT:32'd0;
	wire [31:0] DBUS_LIM = (rdLim)?TLIM:32'd0;
	wire [31:0] DBUS_CTL = (rdCtl)?TCTL:32'd0;
	
	assign DBUSO=(DBUS_CNT | DBUS_LIM | DBUS_CTL);
	
	//keep interrupt request 0 for now
	//assign INTR = 0;
endmodule