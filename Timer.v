module Timer (
	input wire [31:0] ABUS,
	inout wire [31:0] DBUS,
	input wire WE,CLK,LOCK,INIT,CLOCK,
	output wire INTR );
	
	parameter BITS;
	parameter BASE;
	parameter MILLS;
	
	wire selCtl = (ABUS == BASE);
	wire wrCtl = selCtl && WE;
	wire rdCtl = selCtl && (!WE);
	
	//Timer limit register
	reg [4:0] TLIM=0;
	//Timer control register
	reg [4:0] TCTL=0;
	//Timer count register
	reg [4:0] TCNT=0;
	//will change the size of TCTL as needed
	//count of clock cycles to track 1 millisecond
	reg [23:0]count;
	wire checkMilsec = (count==MILLS);
	wire inBounds = (TCNT <= (TLIM-4'd1));
	
	//basically, if it hasn't been a millisecond, feed back in same values. If TCNT is within
	//the TLIM-1 range, increment it. If TLIM is nonzero and TCNT is out of bounds, the read
	//bit is set and TCNT goes back to 0. If the read bit was already set, the overflow bit
	//gets set.
	always @(posedge CLOCK) begin
		count<=checkMilsec?24'd0:count+24'd1;
		if(TLIM!=0) begin
			TCNT<=checkMilsec?inBounds&(TCNT+4'd1):TCNT;
			TCTL<=checkMilsec?{TCTL[0]&!inBounds,!inBounds}:TCTL;
		end else
			TCNT<=checkMilsec?TCNT+4'd1:TCNT;
	end
	
	assign DBUS =	rdCtl?{{28{1'b0}},TCTL}:
						wrCtl?{{24{1'b0}},TCNT}:
						{32{1'bZ}};
	//keep interrupt request 0 for now
	assign INTR = 0;
endmodule