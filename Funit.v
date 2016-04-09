module Funit (
	input [5:0] sreg1, //source register decode
	input [5:0] sreg2, //source register decode
	input			mnop,
	input			anop,
	input			wnop,
	input 		EX_LW,
	input [5:0]	setBusy, //dreg# in decode
	input [5:0]	dreg_W, //the dreg# coming out of WB
	input [5:0] dreg_A,
	input [5:0]	dreg_M,
	output 		drive_A_src1,
	output 		drive_A_src2,
	output 		drive_M_src1,
	output 		drive_M_src2,
	output 		drive_W_src1,
	output 		drive_W_src2,
	output 		stall
	);
	
	
	wire sreg1_in_M = (dreg_M == sreg1 && !mnop && !(dreg_M==6'b000000));
	wire sreg2_in_M = (dreg_M == sreg2 && !mnop  && !(dreg_M==6'b000000));
	wire sreg1_in_A = (dreg_A == sreg1 && !anop  && !(dreg_A==6'b000000));
	wire sreg2_in_A = (dreg_A == sreg2 && !anop  && !(dreg_A==6'b000000));
	wire sreg1_in_W = (dreg_W == sreg1 && !wnop && !(dreg_W==6'b000000));
	wire sreg2_in_W = (dreg_W == sreg2 && !wnop  && !(dreg_W==6'b000000));
	
	
	assign drive_A_src1 = (!EX_LW && sreg1_in_A);
	assign drive_A_src2 = (!EX_LW && sreg2_in_A);
	assign drive_M_src1 = (sreg1_in_M && !sreg1_in_A);
	assign drive_M_src2 = (sreg2_in_M && !sreg2_in_A);
	assign drive_W_src1 = !(drive_M_src1 || drive_A_src1) && sreg1_in_W;
	assign drive_W_src2 = !(drive_M_src2 || drive_A_src2) && sreg2_in_W;
	
	assign stall = (EX_LW && ((sreg1_in_A) || (sreg2_in_A)));
endmodule
