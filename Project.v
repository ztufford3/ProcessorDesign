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
  parameter ADDRSW   =32'hFFFFF090;
  parameter IMEMINITFILE="Sorter3.mif";
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
  
	reg [3:0] oldkey=4'b1111;
	wire key0press={oldkey[0],KEY[0]}==2'b00;
	wire key1press={oldkey[1],KEY[1]}==2'b00;
	wire key2press={oldkey[2],KEY[2]}==2'b00;
	wire key3press={oldkey[3],KEY[3]}==2'b00;
	wire [(DBITS-1):0] keyStore={28'b0,key3press,key2press,key1press,key0press};
	 
	 always @(posedge clk) begin
		oldkey<=KEY;
		//store keypress values at correct location
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
  //reg [(DBITS+1):0] bpred[(DBITS-1):0];
  //assign clk=!KEY[0];
  
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
	// This is the value of "incremented PC", computed in stage 1
	wire [(DBITS-1):0] pcplus_F=PC+INSTSIZE;
	// This is the predicted value of the PC
	// that we used to fetch the next instruction
	//wire [(DBITS+1):0] predval=bpred[PC[(DBITS-1):0]];
	//wire [(DBITS-1):0] prediction=predval[(DBITS-1):0];
	//wire [1:0] predodds=predval[(DBITS+1):(DBITS)];
	//wire [(DBITS-1):0] pcpred_F=(prediction!=0&&predodds>2'd1)?prediction:pcplus_F;
	wire [(DBITS-1):0] pcpred_F=pcplus_F;
	
	// Instruction-fetch
	(* ram_init_file = IMEMINITFILE *)
	reg [(DBITS-1):0] imem[(IMEMWORDS-1):0];
	wire [(DBITS-1):0] inst_F=imem[PC[(IMEMADDRBITS-1):IMEMWORDBITS]];
	
  	// If fetch and decoding stages are the same stage,
	// just connect signals from fetch to decode
	wire [(DBITS-1):0] inst_D=inst_F;
	wire [(DBITS-1):0] pcplus_D=pcplus_F;
	wire [(DBITS-1):0] pcpred_D=pcpred_F;
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
	
	// Register-read
	reg [(DBITS-1):0] regs[(REGWORDS-1):0];
	// Two read ports, always using rs and rt for register numbers
	wire [(REGNOBITS-1):0] rregno1_D=rs_D, rregno2_D=rt_D;
	
		// Now the real data memory
	wire MemEnable=!(memaddr_M[(DBITS-1):DMEMADDRBITS]);
	wire MemWE=(!reset)&wrmem_M&MemEnable;
	(* ram_init_file = IMEMINITFILE, ramstyle="no_rw_check" *)
	reg [(DBITS-1):0] dmem[(DMEMWORDS-1):0];
	always @(posedge clk)
		if(MemWE)
			dmem[memaddr_M[(DMEMADDRBITS-1):DMEMWORDBITS]]<=wmemval_M;
	wire [(DBITS-1):0] MemVal=MemWE?{DBITS{1'bX}}:dmem[memaddr_M[(DMEMADDRBITS-1):DMEMWORDBITS]];
	// Connect memory and input devices to the bus
	wire [(DBITS-1):0] memout_M=
		MemEnable?MemVal:
		(memaddr_M==ADDRKEY)?{12'b0,~KEY}:
		(memaddr_M==ADDRSW)? { 6'b0,SW}:
		32'hDEADDEAD;
	
	// TODO: Decide what gets written into the destination register (wregval_M),
	// when it gets written (wrreg_M) and to which register it gets written (wregno_M)
	// !!!!current selpcplus_D won't work with branches
	wire [(DBITS-1):0] wregval_M=(selaluout_M&&!selmemout_M)?aluout_M:selmemout_M?memout_M:selpcplus_M?pcplus_M:{(DBITS){1'bX}};
	
	always @(posedge clk)
		if(wrreg_W&&!reset)
			regs[wregno_W]<=wregval_W;

	// TODO: Get these signals to the ALU somehow
	//aluin's handle WAR data hazard
	reg [(OP2BITS-1):0] alufunc_A;
	wire signed [(DBITS-1):0] aluin1_A=(isjump_A)?pcplus_A:regval1_A;
	wire signed [(DBITS-1):0] aluin2_A=aluimm_A?workingimm_A:regval2_A;
	
	reg signed [(DBITS-1):0] aluout_A;
	always @(alufunc_A or aluin1_A or aluin2_A)
	case(alufunc_A)
		OP2_EQ,OP1_BEQ:  aluout_A={31'b0,aluin1_A==aluin2_A};
		OP2_LT,OP1_BLT:  aluout_A={31'b0,aluin1_A< aluin2_A};
		OP2_LE,OP1_BLE:  aluout_A={31'b0,aluin1_A<=aluin2_A};
		OP2_NE,OP1_BNE:  aluout_A={31'b0,aluin1_A!=aluin2_A};
		OP2_ADD: aluout_A=aluin1_A+aluin2_A;
		OP2_AND,OP1_ANDI: aluout_A=aluin1_A&aluin2_A;
		OP2_OR:  aluout_A=aluin1_A|aluin2_A;
		OP2_XOR: aluout_A=aluin1_A^aluin2_A;
		OP2_SUB: aluout_A=aluin1_A-aluin2_A;
		OP2_NAND:aluout_A=~(aluin1_A&aluin2_A);
		OP2_NOR: aluout_A=~(aluin1_A|aluin2_A);
		OP2_NXOR:aluout_A=~(aluin1_A^aluin2_A);
		default: aluout_A={DBITS{1'bX}};
	endcase
	
	//do we actually need stall with branch prediction?
	wire dobranch_A=(isbranch_A&&(aluout_A==1));	//TODO
	//PC+4+4*sxt(imm)
	
	wire [(DBITS-1):0] brtarg_A=pcplus_A+workingimm_A;	//TODO
	//same address computation as branch - need ALU for other part of instr
	wire [(DBITS-1):0] jmptarg_A=regval1_A+workingimm_A;	//TODO
	
	reg [(DBITS-1):0] pcpred_A;	//TODO
	
	// TODO: Generate the dobranch, brtarg, isjump, and jmptarg signals somehow...
	wire [(DBITS-1):0] pcgood_A=
		dobranch_A?brtarg_A:
		isjump_A?jmptarg_A:
		pcplus_A;
	wire mispred_A=(pcgood_A!=pcpred_A);
	wire mispred_B=mispred_A&&!isnop_A;
	wire [(DBITS-1):0] pcgood_B=pcgood_A;
	/*
	wire [1:0] newpredodds=mispred_B?predodds_A+2'd1:predodds_A;
	integer i;
	always @(posedge clk or posedge reset) begin
		if(reset) begin
		for(i=0;i<(1<<(DBITS-1));i=i+1)
			bpred[i]<=34'd0;
		end else
			if(!flush_A) begin
				bpred[PC_A[(DBITS-1):0]] <= {newpredodds,pcgood_A};
			end
	end*/
	
	// TODO: This is a good place to generate the flush_? signals
	
	wire flush_D=(mispred_B|isjump_A);
	
	// TODO: Write code that produces wmemval_M, wrmem_M, wrreg_M, etc.
	//store value from source register two
		//TODO
	reg [(DBITS-1):0] memaddr_M;
	wire [(DBITS-1):0] wmemval_M=wrmem_M?regval2_M:{(DBITS){1'bX}};
	always @(posedge clk)
		if(wrmem_A|selmemout_A)
			memaddr_M<=aluout_A;
		else
			memaddr_M<={(DBITS){1'bX}};
	
	reg [(DBITS-1):0] aluout_M,pcplus_M;
	always @(posedge clk)
		{aluout_M,pcplus_M}<=
		{aluout_A,pcplus_A};
	
	// Create and connect HEX register
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
	// TODO: Write the code for LEDR here
			if(wrmem_M&&(memaddr_M==ADDRLEDR))
				LedrOut <= wmemval_M[9:0];
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
			// TODO: Write the rest of the decoding code
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
		//ensure ALU registers don't get clobbered when new instruction is decoded next cycle
	end
	
	//update regs for Exec/ALU stage
	reg [(DBITS-1):0] regval1_A;
	reg [(DBITS-1):0] regval2_A;
	reg [(DBITS-1):0] regval1_M;
	reg [(DBITS-1):0] regval2_M;
	reg [(DBITS-1):0] regval1_W;
	reg [(DBITS-1):0] regval2_W;
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
	reg selaluout_W;
	reg selpcplus_A;
	reg selpcplus_M;
	reg selpcplus_W;
	reg selmemout_A;
	reg selmemout_M;
	reg selmemout_W;
	reg isnop_A = 1'b1;
	reg isnop_M = 1'b1;
	reg isnop_W = 1'b1;
	reg [(DBITS-1):0] pcplus_A;
	reg [(DBITS-1):0] pcplus_W;
	reg signed [(DBITS-1):0] workingimm_A;
	reg signed [(DBITS-1):0] workingimm_M; //just for debugging
	reg [(REGNOBITS-1):0] dreg_A={REGNOBITS{1'b0}};
	reg [(REGNOBITS-1):0] dreg_M={REGNOBITS{1'b0}};
	reg [(REGNOBITS-1):0] dreg_W={REGNOBITS{1'b0}};
	reg [(REGNOBITS-1):0] sreg1_A;
	reg [(REGNOBITS-1):0] sreg2_A;
	reg [(REGNOBITS-1):0] sreg1_M;
	reg [(REGNOBITS-1):0] sreg2_M;
	reg [(REGNOBITS-1):0] sreg1_W;
	reg [(REGNOBITS-1):0] sreg2_W;
	reg [(DBITS-1):0] wregval_W;
	wire [(REGNOBITS-1):0] dreg_D=(aluimm_D|isjump_D)?rt_D:rd_D;
	reg [(DBITS-1):0] PC_A;
	reg [(DBITS-1):0] PC_M;
	reg flush_A;
	reg [1:0] predodds_A;
	
	always @(posedge clk) begin
		//predodds_A<=predodds;
		//initial regval1_A and regval2_A assignments handle RAW data hazard
		case (sreg1_mux)
			2'b11:
				regval1_A<=wregval_W;
			2'b10 :
				regval1_A<=wregval_M;
			2'b01 :
				regval1_A<=aluout_A;
			2'b00 :
				regval1_A<=regs[rregno1_D];
		endcase
		regval1_M<=regval1_A;
		regval1_W<=regval1_M;
		case (sreg2_mux)
			2'b11:
				regval2_A<=wregval_W;
			2'b10 :
				regval2_A<=wregval_M;
			2'b01 :
				regval2_A<=aluout_A;
			2'b00 :
				regval2_A<=regs[rregno2_D];
		endcase
		
		regval2_M<=regval2_A;
		regval2_W<=regval2_M;
		wregno_A<=(stall)?0:wregno_D;
		wregno_M<=wregno_A;
		wregno_W<=wregno_M;
		wrreg_A<=(stall)?0:wrreg_D;
		wrreg_M<=wrreg_A;
		wrreg_W<=wrreg_M;
		wrmem_A<=(stall)?0:wrmem_D;
		wrmem_M<=wrmem_A;
		wrmem_W<=wrmem_M;
		isbranch_A<=(stall)?0:isbranch_D;
		isjump_A<=(stall)?0:isjump_D;
		selaluout_A<=(stall)?0:selaluout_D;
		selaluout_M<=selaluout_A;
		selaluout_W<=selaluout_M;
		selmemout_A<=(stall)?0:selmemout_D;
		selmemout_M<=selmemout_A;
		selmemout_W<=selmemout_M;
		selpcplus_A<=(stall)?0:selpcplus_D;
		selpcplus_M<=selpcplus_A;
		selpcplus_W<=selpcplus_M;
		isnop_A<=(stall)?1'b1:isnop_D;
		isnop_M<=isnop_A;
		isnop_W<=isnop_M;
		pcplus_A<=(stall)?0:pcplus_D;
		pcplus_W<=pcplus_M;
		workingimm_A<=(stall)?0:workingimm_D;
		workingimm_M<=workingimm_A;
		alufunc_A<=alufunc_D;
		dreg_A<=(stall)?0:dreg_D;
		dreg_M<=dreg_A;
		dreg_W<=dreg_M;
		sreg1_A<=(stall)?0:rs_D;
		sreg1_M<=sreg1_A;
		sreg1_W<=sreg1_M;
		sreg2_A<=(stall)?0:rt_D;
		sreg2_M<=sreg2_A;
		sreg2_W<=sreg2_M;
		aluimm_A<=(stall)?0:aluimm_D;
		wregval_W<=wregval_M;
		PC_A<=PC;
		PC_M<=PC_A;
		pcpred_A<=pcpred_D;
		flush_A<=flush_D;
	end
	wire mnop = (isnop_M |wrmem_M);
	wire anop = (isnop_A |wrmem_A);
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
	 assign sreg1_mux[0]= (A1 || W1);
	 assign sreg1_mux[1]= (M1 || W1);
	 assign sreg2_mux[0]= (A2 || W2);
	 assign sreg2_mux[1]= (M2 || W2);
  Funit forward (
    .sreg1		(rregno1_D),
	 .sreg2		(rregno2_D),
	 .mnop		(mnop),
	 .anop		(anop),
	 .wnop		(wnop),
	 .EX_LW		(selmemout_A),
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