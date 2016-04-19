module Project(
	input        CLOCK_50,
	input        RESET_N,
	input  [3:0] KEY,
	input  [9:0] SW,
	output [6:0] HEX0,
	output [6:0] HEX1,
	output [6:0] HEX2,
	output [6:0] HEX3,
	output [6:0] HEX4,
	output [6:0] HEX5,
	output [9:0] LEDR
);

  parameter DBITS    =32;
  parameter INSTSIZE =32'd4;
  parameter INSTBITS =32;
  parameter REGNOBITS=6;
  parameter REGWORDS=(1<<REGNOBITS);
  parameter IMMBITS  =14;
  parameter STARTPC  =32'h100;
  parameter ADDRHEX  =32'hFFFFF000;
  parameter ADDRLEDR =32'hFFFFF020;
  parameter ADDRKEY  =32'hFFFFF080;
  parameter ADDRKEYCTRL =32'hFFFFF084;
  parameter ADDRSW   =32'hFFFFF090;
  parameter ADDRSWCTRL	=32'hFFFFF094;
  parameter ADDRTCNT	=32'hFFFFF100;
  parameter ADDRTLIM	=32'hFFFFF104;
  parameter ADDRTCTL	=32'hFFFFF108;
  parameter IMEMINITFILE="clock.mif";
  parameter IMEMADDRBITS=16;
  parameter IMEMWORDBITS=2;
  parameter IMEMWORDS=(1<<(IMEMADDRBITS-IMEMWORDBITS));
  parameter DMEMADDRBITS=16;
  parameter DMEMWORDBITS=2;
  parameter DMEMWORDS=(1<<(DMEMADDRBITS-DMEMWORDBITS));
  
  parameter OP1BITS=6;
  parameter OP1_ALUR =6'b000000;
  parameter OP1_BEQ  =6'b001000;
  parameter OP1_BLT  =6'b001001;
  parameter OP1_BLE  =6'b001010;
  parameter OP1_BNE  =6'b001011;
  parameter OP1_JAL  =6'b001100;
  parameter OP1_LW   =6'b010010;
  parameter OP1_SW   =OP1_LW+6'b001000;
  parameter OP1_ADDI =6'b100000;
  parameter OP1_ANDI =6'b100100;
  parameter OP1_ORI  =6'b100101;
  parameter OP1_XORI =6'b100110;
  parameter ADDROP	=3'b001;
  
  parameter OP2BITS=6;
  parameter OP2_EQ   =OP1_BEQ;
  parameter OP2_LT   =OP1_BLT;
  parameter OP2_LE   =OP1_BLE;
  parameter OP2_NE   =OP1_BNE;
  parameter OP2_ADD  =OP1_ADDI;
  parameter OP2_AND  =OP1_ANDI;
  parameter OP2_OR   =OP1_ORI;
  parameter OP2_XOR  =OP1_XORI;
  parameter OP2_SUB  =OP2_ADD|6'b001000;
  parameter OP2_NAND =OP2_AND|6'b001000;
  parameter OP2_NOR  =OP2_OR |6'b001000;
  parameter OP2_NXOR =OP2_XOR|6'b001000;
  
  //clock frequency
  parameter FREQ = 32'd70;
  parameter MILLISEC = FREQ*24'd1000;
  
	reg [3:0] oldkey=4'b1111;
	wire key0press={oldkey[0],KEY[0]}==2'b00;
	wire key1press={oldkey[1],KEY[1]}==2'b00;
	wire key2press={oldkey[2],KEY[2]}==2'b00;
	wire key3press={oldkey[3],KEY[3]}==2'b00;
	
	reg [3:0] KCTRL = 4'd0;
	reg [3:0] KDATA = 4'd0;
	
	//high if KDATA change is detected
	wire key_ready=(KEY!=KDATA);
	//set high if ready is still 1 when KDATA changes
	wire key_overrun=key_ready&&(KCTRL[0]);
	//control bit. 0 for now
	wire key_ie=0;
	
	 always @(posedge clk) begin
		oldkey<=KEY;
		//...bit 4 is ie? Does he mean bit 3? Is a bit just always 0? Should this just be 3 bits?
		KCTRL<={key_ie,1'b0,key_overrun,key_ready};
		KDATA<={key3press,key2press,key1press,key0press};
		//read from KDATA sets ready bit to 0
		if(memaddr_M==ADDRKEY && selmemout_M)
			KCTRL[0]<=1'b0;
	  end
  
  // The reset signal comes from the reset button on the DE0-CV board
  // RESET_N is active-low, so we flip its value ("reset" is active-high)
  wire clk,locked;
  // The PLL is wired to produce clk and locked signals for our logic
  Pll myPll(
    .refclk(CLOCK_50),
	 .rst      (!RESET_N),
	 .outclk_0 (clk),
    .locked   (locked)
  );
  wire reset=!locked;
  reg [(DBITS-1):0] bpred [64:0];
  //assign clk=!KEY[0];
  
  //made switch debouncer arbitrarily large
  reg [23:0] switch_debouncer;
  reg [9:0] SDATA;
  reg [3:0] SCTRL;
  //1 when a switch value changes
  wire switch_ready=(SDATA!=SW);
  //if the switch value has changed, but ready is still high
  wire switch_overrun=(switch_ready)&&(SDATA[0]);
  //for interrupts. 0 for now
  wire switch_control = 0;
  
  always @(posedge clk or posedge reset) begin
	  if(reset)
		switch_debouncer<=24'd0;
	  else begin
			if(switch_ready) //begin debouncing whenever any switch value changes and remains changed
				switch_debouncer<=switch_debouncer+24'd1;
			else //when value is stable, no need to debounce
				switch_debouncer<=24'd0;
			//if we're not udpating the switch value on debounce, if switch is read we should 
			//clear the ready bit
			if(switch_debouncer>=(MILLISEC*24'd10)) begin
				//wondering what's going on with bits #2 and #3 here as well
				SCTRL<={switch_control, 1'b0, switch_overrun, switch_ready};
				SDATA<={SW};
				switch_debouncer<=24'd0;
			end else if(memaddr_M==ADDRSW && selmemout_M)
				SDATA[0]<=1'b0;
		end
	end
  
	// The PC register and update logic
	reg  [(DBITS-1):0] PC=STARTPC;
	always @(posedge clk) begin
	if(reset)
		PC<=STARTPC;
	else if(mispred_B && !stall)
		PC<=pcgood_B;
	else if(!stall)
		PC<=pcpred_F;
	end
	
	wire [5:0] PC_X=PC[7:2];
	// This is the value of "incremented PC", computed in stage 1
	wire [(DBITS-1):0] pcplus_F=PC+INSTSIZE;
	// This is the predicted value of the PC
	// that we used to fetch the next instruction
	wire [(DBITS-1):0] prediction=bpred[PC_X];
	wire [(DBITS-1):0] pcpred_F=((prediction!=32'd0)&&(inst_F[31:29]==ADDROP))?prediction:pcplus_F;
	
	// Instruction-fetch
	(* ram_init_file = IMEMINITFILE *)
	reg [(DBITS-1):0] imem[(IMEMWORDS-1):0];
	wire [(DBITS-1):0] inst_F=imem[PC[(IMEMADDRBITS-1):IMEMWORDBITS]];
	
  	// If fetch and decoding stages are the same stage,
	// just connect signals from fetch to decode
	reg [(DBITS-1):0] inst_D;
	reg [(DBITS-1):0] pcplus_D;
	reg [(DBITS-1):0] pcpred_D;
	// Instruction decoding
	// These have zero delay from inst_D
	// because they are just new names for those signals
	wire [(OP1BITS-1):0]   op1_D=inst_D[(DBITS-1):(DBITS-OP1BITS)];
	wire [(REGNOBITS-1):0] rs_D,rt_D,rd_D;
	assign {rs_D,rt_D,rd_D}=inst_D[(DBITS-OP1BITS-1):(DBITS-OP1BITS-3*REGNOBITS)];
	wire [(OP2BITS-1):0] op2_D=inst_D[(OP2BITS-1): 0];
	wire [(IMMBITS-1):0] rawimm_D=inst_D[(IMMBITS-1):0];
	reg [(OP2BITS-1):0] alufunc_D;
	reg signed [(DBITS-1):0] workingimm_D;
	reg aluimm_D;
	reg selaluout_D;
	reg selmemout_D;
	reg selpcplus_D;
	reg isbranch_D;
	reg isjump_D;
	reg isnop_D;
	reg wrmem_D;
	reg [(REGNOBITS-1):0] wregno_D;
	reg wrreg_D;
	wire stall_F;
	
	always @(posedge clk) begin
		inst_D<=(!stall)?inst_F:inst_D;
		pcpred_D<=(!stall)?pcpred_F:pcpred_D;
		pcplus_D<=(!stall)?pcplus_F:pcplus_D;
		flush_F<=mispred_B&!isnop_M;
	end
	
	// Register-read
	reg [(DBITS-1):0] regs[(REGWORDS-1):0];
	// Two read ports, always using rs and rt for register numbers
	wire [(REGNOBITS-1):0] rregno1_D=rs_D, rregno2_D=rt_D;
	
		// Now the real data memory
	wire MemEnable=!(memaddr_M[(DBITS-1):DMEMADDRBITS]);
	wire MemWE=(!reset)&wrmem_M&MemEnable;
	(* ram_init_file = IMEMINITFILE, ramstyle="no_rw_check" *)
	reg [(DBITS-1):0] dmem[(DMEMWORDS-1):0];
	
	wire [(DBITS-1):0] wmemval_M=wrmem_M?regval2_M:{(DBITS){1'bX}};
	
	//wire selCnt = ;
	//wire selLim = ;
	//wire selCtl = ;
	wire wrCnt = (memaddr_M == ADDRTCNT) && wrmem_M;
	//wire rdCnt = selCnt && (!wrmem_M);
	wire wrLim = (memaddr_M == ADDRTLIM) && wrmem_M;
	//wire rdLim = selLim && (!wrmem_M);
	wire wrCtl = (memaddr_M == ADDRTCTL) && wrmem_M;
	//wire rdCtl = selCtl && (!wrmem_M);
	
	//Timer limit register
	reg [31:0] TCNT=32'b0;
	//Timer control register
	reg [4:0] TCTL=4'b0;
	//Timer count register
	reg [31:0] TLIM=32'b0;
	reg [31:0] count=32'b0;
	//will change the size of TCTL as needed
	//count of clock cycles to track 1 millisecond
	wire checkMilsec = ((count%(MILLISEC-1))==0);
	wire limEn = (TLIM!=0);
	wire limHit = TCNT==(TLIM-1);
	
	//basically, if it hasn't been a millisecond, feed back in same values. If TCNT is within
	//the TLIM-1 range, increment it. If TLIM is nonzero and TCNT is out of bounds, the read
	//bit is set and TCNT goes back to 0. If the read bit was already set, the overflow bit
	//gets set.
	always @(posedge clk) begin
		count<=count+32'd1;
		TCTL<=(wrCtl)?TCTL&wmemval_M[3:0]:
					(limHit&limEn&TCTL[0])?4'b0011:
					(limHit&limEn)?4'b0001:
					TCTL;
		//TCTL[0]<=(wrCtl)?1'b0:
					
					//TCTL[0];
		
		TCNT<=	(wrCnt)?wmemval_M:
					(limHit&&limEn)?32'b0:
					checkMilsec?(TCNT+32'd1):
					TCNT;
		TLIM<=(wrLim)?wmemval_M:TLIM;
		//TCTL<=(wrCtl)?DBUSI:TCTL;
	end
	
	//wire [31:0] DBUS_CNT = (rdCnt)?TCNT:32'd0;
	//wire [31:0] DBUS_LIM = (rdLim)?TLIM:32'd0;
	//wire [31:0] DBUS_CTL = (rdCtl)?{28'b0,TCTL}:32'd0;
	
	//wire [31:0] tout_M=(DBUS_CNT | DBUS_LIM | DBUS_CTL);
	/*Timer timer(	.ABUS(memaddr_M),
						.DBUSI(wmemval_M),
						.WE(wrmem_M),
						.CLK(clk),
						.DBUSO(tout_M) ); */
						
	always @(posedge clk)
		if(MemWE)
			dmem[memaddr_M[(DMEMADDRBITS-1):DMEMWORDBITS]]<=wmemval_M;
	wire [(DBITS-1):0] MemVal=MemWE?{DBITS{1'bX}}:dmem[memaddr_M[(DMEMADDRBITS-1):DMEMWORDBITS]];
	// Connect memory and input devices to the bus
	wire [(DBITS-1):0] memout_M=
		MemEnable?MemVal:
		(memaddr_M==ADDRKEY)?{12'b0,KDATA}:
		(memaddr_M==ADDRSW)? {6'b0,SDATA}:
		(memaddr_M==ADDRHEX)?{8'b0,HexOut}:
		(memaddr_M==ADDRLEDR)?{22'b0,LedrOut}:
		(memaddr_M==ADDRTCNT)?TCNT:
		(memaddr_M==ADDRTLIM)?TLIM:
		(memaddr_M==ADDRTCTL)?{28'b0,TCTL}:
		32'hDEADDEAD;
	
	// If LW is executed with the address targeting Hex or LEDR, it will grab the current output
	wire [(DBITS-1):0] wregval_M=(selaluout_M&&!selmemout_M)?aluout_M:
											selmemout_M?memout_M:
											selpcplus_M?pcplus_M:
											{(DBITS){1'bX}};
	
	always @(posedge clk)
		if(wrreg_W&&!reset)
			regs[wregno_W]<=wregval_W;

	//aluin's handle WAR data hazard
	reg [(OP2BITS-1):0] alufunc_A;
	wire signed [(DBITS-1):0] aluin1_A=(isjump_A)?pcplus_A:regval1_A;
	wire signed [(DBITS-1):0] aluin2_A=aluimm_A?workingimm_A:regval2_A;
	wire signed [(DBITS-1):0] alu_log;
	wire signed [(DBITS-1):0] alu_eq;
	wire signed [(DBITS-1):0] alu_mth;
	wire signed [(DBITS-1):0] aluout_A;
	
	assign alu_eq=	(alufunc_A==OP2_EQ)?{31'b0,aluin1_A==aluin2_A}:
					(alufunc_A==OP2_NE)?{31'b0,aluin1_A!=aluin2_A}:
					(alufunc_A==OP2_LT)?{31'b0,aluin1_A<aluin2_A}:
					(alufunc_A==OP2_LE)?{31'b0,aluin1_A<=aluin2_A}:
					{32'b0};
	assign alu_log=	(isbranch_A)?{32'b0}:
					(alufunc_A==OP2_AND)?{aluin1_A&aluin2_A}:
					(alufunc_A==OP2_OR)?{aluin1_A|aluin2_A}:
					(alufunc_A==OP2_XOR)?{aluin1_A^aluin2_A}:
					(alufunc_A==OP2_NAND)?{~(aluin1_A&aluin2_A)}:
					(alufunc_A==OP2_NOR)?{~(aluin1_A|aluin2_A)}:
					(alufunc_A==OP2_NXOR)?{~(aluin1_A^aluin2_A)}:
					{32'b0};
	assign alu_mth=	(isbranch_A)?{32'b0}:
					(alufunc_A==OP2_ADD)?{aluin1_A+aluin2_A}:
					(alufunc_A==OP2_SUB)?{aluin1_A-aluin2_A}:
					{32'b0};
	
	assign aluout_A = {alu_eq|alu_log|alu_mth};
	
	wire dobranch_A=(isbranch_A&&(aluout_A==1));
	
	wire [(DBITS-1):0] brtarg_A=pcplus_A+workingimm_A;
	//same address computation as branch - need ALU for other part of instr
	wire [(DBITS-1):0] jmptarg_A=regval1_A+workingimm_A;
	
	reg [(DBITS-1):0] pcpred_A;
	
	wire [(DBITS-1):0] pcgood_M=
		dobranch_M?brtarg_M:
		isjump_M?jmptarg_M:
		pcplus_M;
	wire mispred_M=(pcgood_M!=pcpred_M);
	wire mispred_B=mispred_M&&!isnop_M;
	wire [(DBITS-1):0] pcgood_B=pcgood_M;
	
	reg [(DBITS-1):0] brtarg_M;
	reg [(DBITS-1):0] jmptarg_M;
	reg dobranch_M;
	reg isjump_M;
	
	always @(posedge clk) begin
		brtarg_M<=(!flush_A)?brtarg_A:0;
		jmptarg_M<=(!flush_A)?jmptarg_A:0;
		dobranch_M<=(!flush_A)?dobranch_A:0;
		isjump_M<=(!flush_A)?isjump_A:0;
	end
	
	wire [5:0] PC_Y=PC_W[7:2];
	//wire [1:0] newpredodds=mispred_B?predodds_A+2'd1:predodds_A;
	integer i;
	always @(posedge clk or posedge reset) begin
		if(reset) begin
			for(i=0;i<8;i=i+1)
				bpred[i]<=32'd0;
		end else
			//if(mispred_B_W || prediction_W&&!isnop_W)
			if(prediction_W&&!isnop_W)
				bpred[PC_Y] <= pcgood_W;
	end
	reg flush_F;
	wire flush_D=((mispred_B)&!isnop_M) | flush_F;
	wire flush_A=(mispred_B)&!isnop_M;
	
	reg [(DBITS-1):0] memaddr_M;
	always @(posedge clk)
		if(wrmem_A|selmemout_A)
			memaddr_M<=aluout_A;
		else
			memaddr_M<={(DBITS){1'bX}};
	
	reg [(DBITS-1):0] aluout_M,pcplus_M;
	
	reg [23:0] HexOut;
	reg [9:0] LedrOut;
	SevenSeg ss5(.OUT(HEX5),.IN(HexOut[23:20]));
	SevenSeg ss4(.OUT(HEX4),.IN(HexOut[19:16]));
	SevenSeg ss3(.OUT(HEX3),.IN(HexOut[15:12]));
	SevenSeg ss2(.OUT(HEX2),.IN(HexOut[11:8]));
	SevenSeg ss1(.OUT(HEX1),.IN(HexOut[7:4]));
	SevenSeg ss0(.OUT(HEX0),.IN(HexOut[3:0]));
	
	assign LEDR[9:0] = LedrOut;
	
	always @(posedge clk or posedge reset) begin
		if(reset)
			HexOut<=24'hFEDEAD;
		else begin
			if(wrmem_M&&(memaddr_M==ADDRHEX))
				HexOut[23:0] <= wmemval_M[23:0];
			//if(wrmem_M&&(memaddr_M==ADDRLEDR))
				/*if(wrmem_M&&(memaddr_M==ADDRTCNT)) begin
					HexOut[23:12]<=TCNT[11:0];
					HexOut[11:0]<=wmemval_M[11:0];
					LedrOut[9]<=1'b1;
					LedrOut[7]<=1'b0;
					LedrOut[5]<=1'b0;
					LedrOut[0]<=1'b0;
				end else if(wrmem_M&&(memaddr_M==ADDRTLIM)) begin
					HexOut[23:12]<=TLIM[11:0];
					HexOut[11:0]<=wmemval_M[11:0];
					LedrOut[7]<=1'b1;
					LedrOut[9]<=1'b0;
					LedrOut[5]<=1'b0;
					LedrOut[0]<=1'b0;
				end else if(wrmem_M&&(memaddr_M==ADDRTCTL)) begin
					HexOut[23:12]<={8'b0,TCTL[3:0]};
					HexOut[11:0]<=wmemval_M[11:0];
					LedrOut[5]<=1'b1;
					LedrOut[9]<=1'b0;
					LedrOut[7]<=1'b0;
					LedrOut[0]<=1'b0;
				end else if(rdCnt) begin
					HexOut[23:12]<=TCNT[11:0];
					HexOut[11:0]<=memout_M[11:0];
					LedrOut[9:7]<=3'b111;
					LedrOut[6:0]<=7'b0;
				end else if(rdLim) begin
					HexOut[23:12]<=TLIM[11:0];
					HexOut[11:0]<=memout_M[11:0];
					LedrOut[6:4]<=3'b111;
					LedrOut[9:7]<=3'b0;
					LedrOut[3:0]<=4'b0;
				end else if(rdCtl) begin
					HexOut[23:12]<={8'b0,TCTL[3:0]};
					HexOut[11:0]<=memout_M[11:0];
					LedrOut[3:1]<=3'b111;
					LedrOut[9:4]<=6'b0;
					LedrOut[0]<=1'b0;
				end else begin
					//HexOut[23:12]<={8'b0,TCTL};
					//HexOut[11:0]<=TCNT[11:0];
					HexOut[23:0]<=PC_M[23:0];
					LedrOut<=10'd1;
				end
				
					LedrOut[8]<=wrCnt;
					LedrOut[6]<=wrLim;
					LedrOut[4]<=wrCtl;
												//memaddr_M==ADDRTLIM||
												//memaddr_M==ADDRTCTL))
				  //LedrOut<=10'd1;
				//LedrOut <= wmemval_M[9:0];
				//if(wrmem_M)
					//HexOut[23:0]<=memaddr_M[23:0];*/
		end
	end

	// Decoding logic
	always @* begin
		{aluimm_D,      alufunc_D}=
		{    1'bX,{OP2BITS{1'bX}}};
		{isbranch_D,isjump_D,isnop_D,wrmem_D}=
		{      1'b0,    1'b0,   1'b0,   1'b0};
		{selaluout_D,selmemout_D,selpcplus_D,wregno_D,          wrreg_D,	workingimm_D}=
		{       1'bX,       1'b0,       1'bX,{REGNOBITS{1'bX}},   1'b0,	{(DBITS){1'bX}}};
		if(reset|flush_D)
			isnop_D=1'b1;
		else case(op1_D)
		OP1_ALUR:
			{aluimm_D,alufunc_D,selaluout_D,selmemout_D,selpcplus_D,wregno_D,wrreg_D}=
			{		1'b0,    op2_D,       1'b1,       1'b0,       1'b0,    rd_D,   1'b1};
		OP1_ADDI,OP1_ANDI,OP1_ORI,OP1_XORI:
			{aluimm_D,alufunc_D,selaluout_D,selmemout_D,selpcplus_D,wregno_D,wrreg_D,workingimm_D}=
			{		1'b1,		op1_D,		1'b1,			1'b0,			1'b0,		rt_D,		1'b1,	{{(DBITS-IMMBITS){rawimm_D[IMMBITS-1]}},{rawimm_D}}};		
		OP1_BEQ,OP1_BLT,OP1_BLE,OP1_BNE:
			{aluimm_D,alufunc_D,isbranch_D,selaluout_D,selmemout_D,selpcplus_D,workingimm_D}=
			{		1'b0,		op1_D,		1'b1,			1'b0,			1'b0,			1'b0,{{{(DBITS-IMMBITS){rawimm_D[IMMBITS-1]}},{rawimm_D}}<<2}};
		OP1_JAL:
			{aluimm_D,isjump_D,selaluout_D,selmemout_D,selpcplus_D,wregno_D,wrreg_D,workingimm_D}=
			{		1'b0,		1'b1,			1'b0,			1'b0,			1'b1,		rt_D,		1'b1,	{{{(DBITS-IMMBITS){rawimm_D[IMMBITS-1]}},{rawimm_D}}<<2}};
		OP1_SW:
			{aluimm_D,alufunc_D,selaluout_D,selmemout_D,selpcplus_D,wrmem_D,workingimm_D}=
			{		1'b1,	OP1_ADDI,		1'b1,			1'b0,			1'b0,		1'b1,	{{(DBITS-IMMBITS){rawimm_D[IMMBITS-1]}},{rawimm_D}}};
		OP1_LW:
			{aluimm_D,alufunc_D,selaluout_D,selmemout_D,selpcplus_D,wrreg_D,wregno_D,workingimm_D}=
			{		1'b1,	OP1_ADDI,		1'b1,			1'b1,			1'b1,		1'b1,	rt_D,{{(DBITS-IMMBITS){rawimm_D[IMMBITS-1]}},{rawimm_D}}};
		
		default: ;
		endcase
	end
	
	//update regs for Exec/ALU stage
	reg [(DBITS-1):0] regval1_A;
	reg [(DBITS-1):0] regval2_A;
	reg [(DBITS-1):0] regval2_M;
	reg [(REGNOBITS-1):0] wregno_A;
	reg [(REGNOBITS-1):0] wregno_M;
	reg [(REGNOBITS-1):0] wregno_W;
	reg wrreg_A;
	reg wrreg_M;
	reg wrreg_W;
	reg wrmem_A;
	reg wrmem_M;
	reg wrmem_W;
	reg isbranch_A;
	reg isjump_A;
	reg aluimm_A;
	reg selaluout_A;
	reg selaluout_M;
	reg selpcplus_A;
	reg selpcplus_M;
	reg selmemout_A;
	reg selmemout_M;
	reg isnop_A;
	reg isnop_M;
	reg isnop_W;
	reg [(DBITS-1):0] pcplus_A;
	reg signed [(DBITS-1):0] workingimm_A;
	reg [(REGNOBITS-1):0] dreg_A={REGNOBITS{1'b0}};
	reg [(REGNOBITS-1):0] dreg_M={REGNOBITS{1'b0}};
	reg [(REGNOBITS-1):0] dreg_W={REGNOBITS{1'b0}};
	reg [(REGNOBITS-1):0] sreg1_A;
	reg [(REGNOBITS-1):0] sreg2_A;
	reg [(DBITS-1):0] wregval_W;
	wire [(REGNOBITS-1):0] dreg_D=(aluimm_D|isjump_D)?rt_D:rd_D;
	reg [(DBITS-1):0] PC_D;
	reg [(DBITS-1):0] PC_A;
	reg [(DBITS-1):0] PC_M;
	reg [(DBITS-1):0] PC_W;
	reg [(DBITS-1):0] pcgood_W;
	reg [(DBITS-1):0] pcpred_M;
	reg [(OP1BITS-1):0] opcode_A;
	reg [(OP1BITS-1):0] opcode_M;
	reg prediction_W;
	reg prediction_M;
	
	always @(posedge clk) begin
		prediction_M<=(isjump_A || isbranch_A)&&!isnop_A;
		prediction_W<=prediction_M&&!isnop_M;
	
		//initial regval1_A and regval2_A assignments handle RAW data hazard
		regval1_A<=	(flush_D)?0:
						(sreg1_mux==2'b0)?regs[rregno1_D]:
						(sreg1_mux==2'b01)?aluout_A:
						(sreg1_mux==2'b10)?wregval_M:
						wregval_W; //assume sreg1_mux==11
		
		regval2_A<=	(flush_D)?0:
						(sreg2_mux==2'b0)?regs[rregno2_D]:
						(sreg2_mux==2'b01)?aluout_A:
						(sreg2_mux==2'b10)?wregval_M:
						wregval_W; //assume sreg2_mux==11
		
		regval2_M<=regval2_A;
		wregno_A<=(!stall)?wregno_D:0;
		wregno_M<=(!flush_A)?wregno_A:0;
		wregno_W<=wregno_M;
		wrreg_A<=(!stall)?wrreg_D:0;
		wrreg_M<=(!flush_A)?wrreg_A:0;
		wrreg_W<=wrreg_M;
		wrmem_A<=(!stall)?wrmem_D:0;
		wrmem_M<=(!flush_A)?wrmem_A:0;
		wrmem_W<=wrmem_M;
		isbranch_A<=(!stall)?isbranch_D:0;
		isjump_A<=(!stall)?isjump_D:0;
		selaluout_A<=(!stall)?selaluout_D:0;
		selaluout_M<=(!flush_A)?selaluout_A:0;
		selmemout_A<=(!stall)?selmemout_D:0;
		selmemout_M<=(!flush_A)?selmemout_A:0;
		selpcplus_A<=(!stall)?selpcplus_D:0;
		selpcplus_M<=(!flush_A)?selpcplus_A:0;
		isnop_A<=(!stall)?isnop_D:1'b1;
		isnop_M<=(!flush_A)?isnop_A:1'b1;
		isnop_W<=isnop_M;
		pcplus_A<=pcplus_D;
		pcplus_M<=pcplus_A;
		workingimm_A<=workingimm_D;
		alufunc_A<=(!stall)?alufunc_D:0;
		dreg_A<=(!stall)?dreg_D:0;
		dreg_M<=(!flush_A)?dreg_A:0;
		dreg_W<=dreg_M;
		sreg1_A<=(!stall)?rs_D:0;
		sreg2_A<=(!stall)?rt_D:0;
		aluimm_A<=(!stall)?aluimm_D:0;
		wregval_W<=wregval_M;
		PC_D<=PC;
		PC_A<=PC_D;
		PC_M<=PC_A;
		PC_W<=PC_M;
		pcpred_A<=pcpred_D;
		pcpred_M<=pcpred_A;
		pcgood_W<=pcgood_M;
		aluout_M<=aluout_A;
	end
	wire mnop = (isnop_M |wrmem_M);
	wire anop = (isnop_A |wrmem_A	|flush_A);
	wire wnop = (isnop_W |wrmem_W);
  wire [1:0] sreg1_mux; //0 should drive regfile, 1 should drive aluout and 2 should drive Memout
  wire [1:0] sreg2_mux;
  wire stall;
  wire A1;
  wire A2;
  wire M1;
  wire M2;
  wire W1;
  wire W2;
	 assign sreg1_mux= {(M1 || W1),(A1 || W1)};
	 assign sreg2_mux= {(M2 || W2),(A2 || W2)};
	 
  Funit forward (
    .sreg1		(rregno1_D),
	 .sreg2		(rregno2_D),
	 .mnop		(mnop),
	 .anop		(anop),
	 .wnop		(wnop),
	 .EX_LW		(selmemout_A&&!flush_D),
	 .setBusy	(dreg_D),
	 .dreg_W		(dreg_W),
	 .dreg_A		(dreg_A),
	 .dreg_M		(dreg_M),
	 .drive_A_src1	(A1),
	 .drive_A_src2	(A2),
	 .drive_M_src1	(M1),
	 .drive_M_src2	(M2),
	 .drive_W_src1	(W1),
	 .drive_W_src2	(W2),
	 .stall	(stall)
  );
endmodule