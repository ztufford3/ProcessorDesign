module Timer (
	input wire [31:0] ABUS,
	inout wire [31:0] DBUS,
	input wire WE,CLK,LOCK,INIT,
	output wire INTR );
	
	parameter BITS = 32;
	parameter TCNTADDR = 32'hFFFFF100;
	parameter TLIMADDR = 32'hFFFFF104;
	parameter TCTLADDR = 32'hFFFFF100;
	parameter MILLS;
	
	wire selCnt = (ABUS == TCNTADDR);
	wire selLim = (ABUS == TLIMADDR);
	wire selCtl = (ABUS == TCTLADDR);
	wire wrCnt = selCnt && WE;
	wire rdCnt = selCnt && (!WE);
	wire wrLim = selLim && WE;
	wire rdLim = selLim && (!WE);
	wire wrCtl = selCtl && WE && !(DBUS[1] | DBUS[0]);
	wire rdCtl = selCtl && (!WE);
	
	//Timer limit register
	reg [31:0] TCNT=0;
	//Timer control register
	reg [4:0] TCTL=0;
	//Timer count register
	reg [31:0] TLIM=0;
	reg [23:0] count;
	//will change the size of TCTL as needed
	//count of clock cycles to track 1 millisecond
	wire checkMilsec = (count==MILLS-1);
	wire limEn = (TLIM!=0);
	wire limHit = TCNT==TLIM-1;
	
	//basically, if it hasn't been a millisecond, feed back in same values. If TCNT is within
	//the TLIM-1 range, increment it. If TLIM is nonzero and TCNT is out of bounds, the read
	//bit is set and TCNT goes back to 0. If the read bit was already set, the overflow bit
	//gets set.
	always @(posedge CLK) begin
		count<=checkMilsec?24'd0:count+24'd1;
		TCNT<=checkMilsec?(TCNT+4'd1):{(BITS){1'bX}};
		if (limHit & limEn) begin
			TCNT<=0;
			if (TCTL[0]) begin
				TCTL[1]<=1;
			end else begin
				TCTL[0]<=1;
			end
		end
		if (wrCnt) begin 
			TCNT<=DBUS;
		end
		if (wrLim) begin
			TLIM<=DBUS;
		end
		if (wrCtl) begin
			TCTL<=DBUS;
		end
	end
	
	wire [31:0] DBUS_CNT = rdCnt?TCNT:{(BITS){1'bX}};
	wire [31:0] DBUS_LIM = rdLim?TLIM:{(BITS){1'bX}};
	wire [31:0] DBUS_CTL = rdCtl?TCTL:{(BITS){1'bX}};
	
	assign DBUS=(DBUS_CNT | DBUS_LIM | DBUS_CTL);
	
	//keep interrupt request 0 for now
	assign INTR = 0;
endmodule