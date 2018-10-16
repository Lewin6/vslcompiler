#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>


using namespace llvm;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token {
	TOK_EOF = -1,
	ERROR = -2,

	VARIABLE = -3,
	INTEGER = -4,
	TEXT = -5,
	ASSIGN_SYMBOL = -6,

	//commands
	FUNC = -11,
	PRINT = -12,
	RETURN = -13,
	CONTINUE = -14,
	IF = -15,
	THEN = -16,
	ELSE = -17,
	FI = -18,
	WHILE = -19,
	DO = -20,
	DONE = -21,
	VAR = -22,
};

static std::string IdentifierStr; // Filled in if VARIABLE
static int NumVal;             // Filled in if INTERGER
static std::string TextStr;		  // Filled in if TEXT

								  /// gettok - Return the next token from standard input.
static int gettok() {
	static int LastChar = ' ';

	// Skip any whitespace.
	while (isspace(LastChar))
		LastChar = getchar();

	//Commands and VARIABLE
	if (isalpha(LastChar)) {
		if (islower(LastChar)) {
			IdentifierStr = LastChar;
			while (isalnum((LastChar = getchar())))
				IdentifierStr += LastChar;
			return VARIABLE;
		}

		IdentifierStr = LastChar;
		while (isalnum((LastChar = getchar())))
			IdentifierStr += LastChar;

		if (IdentifierStr == "FUNC")
			return FUNC;
		if (IdentifierStr == "PRINT")
			return PRINT;
		if (IdentifierStr == "RETURN")
			return RETURN;
		if (IdentifierStr == "CONTINUE")
			return CONTINUE;
		if (IdentifierStr == "IF")
			return IF;
		if (IdentifierStr == "THEN")
			return THEN;
		if (IdentifierStr == "ELSE")
			return ELSE;
		if (IdentifierStr == "FI")
			return FI;
		if (IdentifierStr == "WHILE")
			return WHILE;
		if (IdentifierStr == "DO")
			return DO;
		if (IdentifierStr == "DONE")
			return DONE;
		if (IdentifierStr == "VAR")
			return VAR;

	}

	//INTERGER
	if (isdigit(LastChar)) {
		std::string NumStr;
		do {
			NumStr += LastChar;
			LastChar = getchar();
		} while (isdigit(LastChar));

		NumVal = atoi(NumStr.c_str());
		return INTEGER;
	}

	//TEXT
	if (LastChar == '/') {
		LastChar = getchar();
		if (LastChar == '/') {
			do
				LastChar = getchar();
			while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
			if (LastChar != EOF)
				return gettok();
		}
		return ERROR;
	}

	//ASSIGN_SYMBOL
	if (LastChar == ':') {
		LastChar = getchar();
		if (LastChar == '=') {
			LastChar = getchar();
			return ASSIGN_SYMBOL;
		}
		return ERROR;
	}
	// Check for end of file.  Don't eat the EOF.
	if (LastChar == '"') {
		IdentifierStr = LastChar;
		while ((LastChar = getchar()) != '"') {
			IdentifierStr += LastChar;
		}
		IdentifierStr += LastChar;
		LastChar = getchar();
		return TEXT;
	}
	if (LastChar == EOF)
		return TOK_EOF;

	// Otherwise, just return the character as its ascii value.
	int ThisChar = LastChar;
	LastChar = getchar();
	return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//

namespace {
	///StatAST - This class for statement
	class StatAST {
	public:
		virtual ~StatAST() = default;
		virtual Value *codegen() = 0;
	};

	/// ExprAST - Base class for all expression nodes.
	class ExprAST {
	public:
		virtual ~ExprAST() = default;
		virtual Value *codegen() = 0;
	};

	///ExprListAST - Expression List
	class ExprListAST {

	public:
		std::vector<std::unique_ptr<ExprAST>> Exps;

		ExprListAST(std::vector<std::unique_ptr<ExprAST>> Exps)
			:Exps(std::move(Exps)) {}
	};

	///ArgListAST - Argument List
	class ArgListAST {
		std::unique_ptr<ExprListAST> Args;

	public:
		ArgListAST(std::unique_ptr<ExprListAST> Exps)
			:Args(std::move(Exps)) {}
	};

	///VarListAST
	class VarListAST {
		std::vector<std::unique_ptr<std::string>> Vars;

	public:
		VarListAST(std::vector<std::unique_ptr<std::string>> Vars)
			:Vars(std::move(Vars)) {}
	};

	/// NumberExprAST - Expression class for numeric literals like "1.0".
	class NumberExprAST : public ExprAST {
		int Val;

	public:
		NumberExprAST(int Val) : Val(Val) {}
		Value *codegen() override;
	};

	/// VariableExprAST - Expression class for referencing a variable, like "a".
	class VariableExprAST : public ExprAST {
		std::string Name;

	public:
		VariableExprAST(const std::string &Name) : Name(Name) {}
		Value *codegen() override;
	};

	/// BinaryExprAST - Expression class for a binary operator.
	class BinaryExprAST : public ExprAST {
		char Op;
		std::unique_ptr<ExprAST> LHS, RHS;

	public:
		BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
			std::unique_ptr<ExprAST> RHS)
			: Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
		Value *codegen() override;
	};

	/// CallExprAST - Expression class for function calls.
	class CallExprAST : public ExprAST {
		std::string Callee;
		std::unique_ptr<ExprListAST> Args;

	public:
		CallExprAST(const std::string &Callee,
			std::unique_ptr<ExprListAST> Args)
			: Callee(Callee), Args(std::move(Args)) {}
		Value *codegen() override;
	};

	/// PrototypeAST - This class represents the "prototype" for a function,
	/// which captures its name, and its argument names (thus implicitly the number
	/// of arguments the function takes).
	class PrototypeAST {
		std::string Name;
		std::vector<std::string> Args;

	public:
		PrototypeAST(const std::string &Name, std::vector<std::string> Args)
			: Name(Name), Args(std::move(Args)) {}
		Function *codegen();
		const std::string &getName() const { return Name; }
	};

	/// FunctionAST - This class represents a function definition itself.
	class FunctionAST {
		std::unique_ptr<PrototypeAST> Proto;
		std::unique_ptr<StatAST> Body;

	public:
		FunctionAST(std::unique_ptr<PrototypeAST> Proto,
			std::unique_ptr<StatAST> Body)
			: Proto(std::move(Proto)), Body(std::move(Body)) {}
		Function *codegen();
	};

	///ProgAST - the program
	class ProgAST {
		std::vector<std::unique_ptr<FunctionAST>> Funs;

	public:
		ProgAST(std::vector<std::unique_ptr<FunctionAST>> Funs)
			:Funs(std::move(Funs)) {}
	};

	///DeclAST - This class for declaration
	class DeclAST {
		std::vector<std::string> Vars;

	public:
		DeclAST(std::vector<std::string> Vars)
			:Vars(std::move(Vars)) {}
	};


	///NullStatAST-Statement class for null statement
	class NullStatAST : public StatAST {
	public:
		Value *codegen() override;
	};

	///ReturnStatAST - Statement class for return statement
	class ReturnStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;

	public:
		ReturnStatAST(std::unique_ptr<ExprAST> Exp)
			:Exp(std::move(Exp)) {}
		Value *codegen() override;
	};

	///AssignStatAST - Statement class for Assignment Statement
	class AssignStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;
		std::string Name;
	public:
		AssignStatAST(const std::string &Name, std::unique_ptr<ExprAST> Exp)
			:Name(Name), Exp(std::move(Exp)) {}
		Value *codegen() override;
	};

	///IfStatAST - Statement class for IF Statement
	class IfStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;
		std::unique_ptr<StatAST> tStat;
		std::unique_ptr<StatAST> eStat;
	public:
		IfStatAST(std::unique_ptr<ExprAST> Exp, std::unique_ptr<StatAST> tStat)
			:Exp(std::move(Exp)), tStat(std::move(tStat)) {
			eStat = nullptr;
		}
		IfStatAST(std::unique_ptr<ExprAST> Exp,
			std::unique_ptr<StatAST> tStat, std::unique_ptr<StatAST> eStat)
			:Exp(std::move(Exp)), tStat(std::move(tStat)), eStat(std::move(eStat)) {}
		Value *codegen() override;
	};

	///WhileStatAST - Statement class for WHILE Statement
	class WhileStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;
		std::unique_ptr<StatAST> Stat;

	public:
		WhileStatAST(std::unique_ptr<ExprAST> Exp,
			std::unique_ptr<StatAST> Stat)
			:Exp(std::move(Exp)), Stat(std::move(Stat)) {}
		Value *codegen() override;
	};

	///BlockStatAST - Statement class for Block Statement "{……}"
	class BlockAST : public StatAST {
		std::vector<std::unique_ptr<DeclAST>> Decls;
		std::vector<std::unique_ptr<StatAST>> Stats;

	public:
		std::vector<std::unique_ptr<StatAST>> getStats() {
			return std::move(Stats);
		}
		BlockAST(std::vector<std::unique_ptr<DeclAST>> Decls,
			std::vector<std::unique_ptr<StatAST>> Stats)
			:Decls(std::move(Decls)), Stats(std::move(Stats)) {}
		Value *codegen() override;
	};
	
	class BlockStatAST : public StatAST {
		//	std::vector<std::unique_ptr<DeclAST>> Decls;
		//	//TODO:
		//	//std::vector<std::unique_ptr<StatAST>> Stats;

		//public:
		//	//BlockStatAST(std::vector<std::unique_ptr<DeclAST>> Decls,
		//	//	std::vector<std::unique_ptr<StatAST>> Stats)
		//	//	:Decls(Decls), Stats(Stats) {}
		//	BlockStatAST(std::vector<std::unique_ptr<DeclAST>> Decls)
		//		:Decls(Decls){}
	};

	//class StatListAST {
	//	std::vector<std::unique_ptr<StatAST>> Stats;

	//public:
	//	StatListAST(std::vector<std::unique_ptr<StatAST>> Stats)
	//		:Stats(Stats) {}
	//};

	///PrintItemAST - class for Print Item Statement
	class PrintItemAST {

	public:
		virtual ~PrintItemAST() = default;
	};

	///ExpPrintItemAST - Print Item class for Expression Print Item
	class ExpPrintItemAST :public PrintItemAST {
		std::unique_ptr<ExprAST> Exp;
		static const int type = 0;
	public:
		ExpPrintItemAST(std::unique_ptr<ExprAST> Exp)
			:Exp(std::move(Exp)) {}
	};

	///TextPrintItemAST - Print Item class for TEXT Print Item
	class TextPrintItemAST :public PrintItemAST {
		std::string Text;
		static const int type = 1;
	public:
		TextPrintItemAST(std::string Text)
			:Text(Text) {}
	};

	///PrintStatAST - Statement class for  Print Item Statement
	class PrintStatAST : public StatAST {
		std::vector<std::unique_ptr<PrintItemAST>> Items;
	public:
		PrintStatAST(std::vector<std::unique_ptr<PrintItemAST>> Items)
			:Items(std::move(Items)) {}
		Value *codegen() override;
	};



} // end anonymous namespace
  //===----------------------------------------------------------------------===//
  // Parser
  //===----------------------------------------------------------------------===//

  /// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
  /// token the parser is looking at.  getNextToken reads another token from the
  /// lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() { return CurTok = gettok(); }

/// BinopPrecedence - This holds the precedence for each binary operator that is
/// defined.
static std::map<char, int> BinopPrecedence;

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
	if (!isascii(CurTok))
		return -1;

	// Make sure it's a declared binop.
	int TokPrec = BinopPrecedence[CurTok];
	if (TokPrec <= 0)
		return -1;
	return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
	LogError(Str);
	return nullptr;
}


static std::unique_ptr<ExprAST> ParseExpression();

/// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
	auto Result = llvm::make_unique<NumberExprAST>(NumVal);
	getNextToken(); // consume the number
	return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
	getNextToken(); // eat (.
	auto V = ParseExpression();
	if (!V)
		return nullptr;

	if (CurTok != ')')
		return LogError("expected ')'");
	getNextToken(); // eat ).
	return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseVariableExpr() {
	std::string IdName = IdentifierStr;

	getNextToken(); // eat identifier.

	if (CurTok != '(') // Simple variable ref.
		return llvm::make_unique<VariableExprAST>(IdName);

	// Call.
	getNextToken(); // eat (
	std::vector<std::unique_ptr<ExprAST>> Args;
	if (CurTok != ')') {
		while (true) {
			if (auto Arg = ParseExpression())
				Args.push_back(std::move(Arg));
			else
				return nullptr;

			if (CurTok == ')')
				break;

			if (CurTok != ',')
				return LogError("Expected ')' or ',' in argument list");
			getNextToken();
		}
	}

	// Eat the ')'.
	getNextToken();

	return llvm::make_unique<CallExprAST>(IdName, std::move(llvm::make_unique<ExprListAST>(std::move(Args))));
}

/// expression with '-'  
//TODO:
static std::unique_ptr<ExprAST>ParseNegaExpr() {
	getNextToken();//eat -.
	auto V = ParseExpression();
	if (!V)
		return nullptr;

	return V;
}


/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
	switch (CurTok) {
	default:
		return LogError("unknown token when expecting an expression");
	case VARIABLE:
		return ParseVariableExpr();
	case INTEGER:
		return ParseNumberExpr();
	case '(':
		return ParseParenExpr();
	case'-':
		return ParseNegaExpr();
	}
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
	std::unique_ptr<ExprAST> LHS) {
	// If this is a binop, find its precedence.
	while (true) {
		int TokPrec = GetTokPrecedence();

		// If this is a binop that binds at least as tightly as the current binop,
		// consume it, otherwise we are done.
		if (TokPrec < ExprPrec)
			return LHS;

		// Okay, we know this is a binop.
		int BinOp = CurTok;
		getNextToken(); // eat binop

						// Parse the primary expression after the binary operator.
		auto RHS = ParsePrimary();
		if (!RHS)
			return nullptr;

		// If BinOp binds less tightly with RHS than the operator after RHS, let
		// the pending operator take RHS as its LHS.
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
			if (!RHS)
				return nullptr;
		}

		// Merge LHS/RHS.
		LHS = llvm::make_unique<BinaryExprAST>(BinOp, std::move(LHS),
			std::move(RHS));
	}
}

/// expression
///   ::= primary binoprhs
///
static std::unique_ptr<ExprAST> ParseExpression() {
	auto LHS = ParsePrimary();
	if (!LHS)
		return nullptr;

	return ParseBinOpRHS(0, std::move(LHS));
}

///AssignStat:  VARIABLE := expression
static std::unique_ptr<StatAST> ParseAssignStat() {
	std::string Vname = IdentifierStr;	//得到变量名
	getNextToken();	//eat variable name
	if (CurTok != ASSIGN_SYMBOL) {
		LogError("Expected ':=' in prototype");
		return nullptr;
	}
	getNextToken();
	if (auto E = ParseExpression())
		return llvm::make_unique<AssignStatAST>(Vname, std::move(E));
	return nullptr;
}

///VarDefinition:	VAR variable_list
static  std::unique_ptr<StatAST> ParseVarDefinition() {
	getNextToken();		//eat VAR


	return nullptr;
}

///PrintStat:	PRINT "text"/expression
static  std::unique_ptr<StatAST> ParsePrintStat() {
	std::vector<std::unique_ptr<PrintItemAST>> Items;
	do {
		if (getNextToken() == TEXT) {
			Items.push_back(llvm::make_unique<TextPrintItemAST>(TextStr));
			getNextToken();
		}
		else {
			if (auto E = ParseExpression())
				Items.push_back(llvm::make_unique<ExpPrintItemAST>(std::move(E)));
			else
			{
				return nullptr;
			}
		}
	} while (CurTok == ',');
	return llvm::make_unique<PrintStatAST>(std::move(Items));
}

///ReturnStat:	RETURN expression
static  std::unique_ptr<StatAST> ParseReturnStat() {
	getNextToken();	//eat RETURN
	if (auto E = ParseExpression())
		return llvm::make_unique<ReturnStatAST>(std::move(E));
	return nullptr;
}

//NullStat:		CONTINUE
static  std::unique_ptr<StatAST> ParseNullStat() {
	getNextToken();	//eat CONTINUE
	return llvm::make_unique<NullStatAST>();
}

static std::unique_ptr<StatAST> ParseStatment();

///IfStat:  IF expression THEN statement FI
///			| IF expression THEN statement
///			  ELSE statement FI
static  std::unique_ptr<StatAST> ParseIfStat() {
	getNextToken();	//eat IF
	if (auto E = ParseExpression()) {
		if (CurTok != THEN) {
			LogError("Expected 'THEN'");
			return nullptr;
		}
		getNextToken();	//eat THEN
		if (auto tS = ParseStatment()) {
			//getNextToken();
			if (CurTok == FI) {
				getNextToken();
				return llvm::make_unique<IfStatAST>(std::move(E), std::move(tS));
			}
			else if (CurTok == ELSE) {
				if (auto eS = ParseStatment()) {
					if (CurTok == FI) {
						getNextToken();
						return llvm::make_unique<IfStatAST>(std::move(E), std::move(tS), std::move(eS));
					}
					else
					{
						LogError("Expected 'FI'");
						return nullptr;
					}
				}
				else
				{
					LogError("Expected Statement");
					return nullptr;
				}
			}
			else
			{
				LogError("Expected 'ELSE' or 'FI'");
				return nullptr;
			}
		}
		return nullptr;
	}
	return nullptr;
}

///WhileStat:  WHILE expression DO statement DONE	
static  std::unique_ptr<StatAST> ParseWhileStat() {
	getNextToken();	//eat WHILE
	if (auto E = ParseExpression()) {
		if (CurTok != DO) {
			LogError("Expected 'DO'");
			return nullptr;
		}
		getNextToken();	//eat DO
		if (auto tS = ParseStatment()) {
			if (CurTok == DONE) {
				getNextToken();
				return llvm::make_unique<WhileStatAST>(std::move(E), std::move(tS));
			}
			else
			{
				LogError("Expected 'DONE'");
				return nullptr;
			}
		}
		return nullptr;
	}
	return nullptr;
}

///Decl: VAR variable_list
static  std::unique_ptr<DeclAST> ParseDecl() {
	if (CurTok != VAR)
		return nullptr;
	std::vector<std::string> Vars;
	do {
		if (getNextToken() == VARIABLE) {
			std::string var = IdentifierStr;
			Vars.push_back(var);
			getNextToken();
		}
		else {
			LogError("Expected VARIABLE after VAR");
			return nullptr;
		}
	} while (CurTok == ',');
	return llvm::make_unique<DeclAST>(std::move(Vars));
}

///BlockStat:	'{' declaration_list statement_list '}'
static  std::unique_ptr<StatAST> ParseBlockStat() {
	getNextToken();	//eat '{'
	std::vector<std::unique_ptr<DeclAST>> Decls;
	std::vector<std::unique_ptr<StatAST>> Stats;
	int i = 0;
	while (CurTok != '}'&& i == 0) {
		if (auto D = ParseDecl()) {
			Decls.push_back(std::move(D));
		}
		else {
			i++;
		}
	}
	while (CurTok != '}' && i == 1) {
		if (auto S = ParseStatment()) {
			Stats.push_back(std::move(S));
		}
		else
		{
			i++;
		}
	}
	if (CurTok == '}') {
		getNextToken();
		return llvm::make_unique<BlockAST>(std::move(Decls), std::move(Stats));
	}
	else
	{
		LogError("Expected '}'");
		return nullptr;
	}
}
//
static std::unique_ptr<StatAST> ParseStatment() {
	switch (CurTok)
	{
	case VARIABLE:
		return ParseAssignStat();
	case PRINT:
		return ParsePrintStat();
	case RETURN:
		return ParseReturnStat();
	case CONTINUE:
		return ParseNullStat();
	case IF:
		return ParseIfStat();
	case WHILE:
		return ParseWhileStat();
	case '{':
		return ParseBlockStat();
	default:
		break;
	}
	return nullptr;
}

/// prototype
///   ::= VARIABLE '(' VARIABLE_LIST ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
	if (CurTok != VARIABLE)
		return LogErrorP("Expected function name in prototype");

	std::string FnName = IdentifierStr;	//得到函数名
	getNextToken();	//eat function name

	if (CurTok != '(')
		return LogErrorP("Expected '(' in prototype");

	std::vector<std::string> ArgNames;
	if (getNextToken() == VARIABLE) {
		ArgNames.push_back(IdentifierStr);
		while (getNextToken() == ',') {
			getNextToken();	//eat ','
			ArgNames.push_back(IdentifierStr);
		}
	}
	if (CurTok != ')')
		return LogErrorP("Expected ')' in prototype");

	// success.
	getNextToken(); // eat ')'.

	return llvm::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

/// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseFuncDefinition() {
	getNextToken(); // eat FUNC.
	auto Proto = ParsePrototype();
	if (!Proto)
		return nullptr;

	if (auto E = ParseStatment())
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
	if (auto E = ParseExpression()) {
		// Make an anonymous proto.
		auto Proto = llvm::make_unique<PrototypeAST>("__anon_expr",
			std::vector<std::string>());
		auto S = llvm::make_unique<AssignStatAST>("__anon_expr", std::move(E));
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(S));
	}
	return nullptr;
}

/// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseFUNC() {
	getNextToken(); // eat FUNC.
	return ParsePrototype();
}


//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;

Value *LogErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
}

Value *NumberExprAST::codegen() {
	return ConstantFP::get(TheContext, APFloat( (float)Val)); 
}

Value *VariableExprAST::codegen() {
	// Look this variable up in the function.
	Value *V = NamedValues[Name];
	if (!V)
		return LogErrorV("Unknown variable name");
	return V;
}

Value *BinaryExprAST::codegen() {
	Value *L = LHS->codegen();
	Value *R = RHS->codegen();
	if (!L || !R)
		return nullptr;

	switch (Op) {
	case '+':
		return Builder.CreateFAdd(L, R, "addtmp");
	case '-':
		return Builder.CreateFSub(L, R, "subtmp");
	case '*':
		return Builder.CreateFMul(L, R, "multmp");
	case '<':
		L = Builder.CreateFCmpULT(L, R, "cmptmp");
		// Convert bool 0/1 to double 0.0 or 1.0
		return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext), "booltmp");
	default:
		return LogErrorV("invalid binary operator");
	}
}

Value *CallExprAST::codegen() {
	// Look up the name in the global module table.
	Function *CalleeF = TheModule->getFunction(Callee);
	if (!CalleeF)
		return LogErrorV("Unknown function referenced");

	// If argument mismatch error.
	if (CalleeF->arg_size() != Args->Exps.size())
		return LogErrorV("Incorrect # arguments passed");

	std::vector<Value *> ArgsV;
	for (unsigned i = 0, e = Args->Exps.size(); i != e; ++i) {
		ArgsV.push_back(Args->Exps[i]->codegen());
		if (!ArgsV.back())
			return nullptr;
	}

	return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Function *PrototypeAST::codegen() {
	// Make the function type:  double(double,double) etc.
	std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(TheContext));
	FunctionType *FT =
		FunctionType::get(Type::getDoubleTy(TheContext), Doubles, false);

	Function *F =
		Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

	// Set names for all arguments.
	unsigned Idx = 0;
	for (auto &Arg : F->args())
		Arg.setName(Args[Idx++]);

	return F;
}

Function *FunctionAST::codegen() {
	// First, check for an existing function from a previous 'extern' declaration.
	Function *TheFunction = TheModule->getFunction(Proto->getName());

	if (!TheFunction)
		TheFunction = Proto->codegen();

	if (!TheFunction)
		return nullptr;

	// Create a new basic block to start insertion into.
	BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
	Builder.SetInsertPoint(BB);

	// Record the function arguments in the NamedValues map.
	NamedValues.clear();
	for (auto &Arg : TheFunction->args())
		NamedValues[Arg.getName()] = &Arg;

	if (Value *RetVal = Body->codegen()) {
		// Finish off the function.
		Builder.CreateRet(RetVal);

		// Validate the generated code, checking for consistency.
		verifyFunction(*TheFunction);

		return TheFunction;
	}

	// Error reading body, remove function.
	TheFunction->eraseFromParent();
	return nullptr;
}

Value * NullStatAST::codegen()
{
	return nullptr;
}

//TODO:
Value * ReturnStatAST::codegen()
{
	return nullptr;
}

Value * AssignStatAST::codegen()
{
	if (Value *RetVal = Exp->codegen()) {
		return Builder.CreateFAdd(llvm::make_unique<NumberExprAST>(0)->codegen(), RetVal, Name);
	}
	return nullptr;
}

//TODO:
Value * IfStatAST::codegen()
{
	return nullptr;
}

//TODO:
Value * WhileStatAST::codegen()
{
	return nullptr;
}


Value * BlockAST::codegen()
{
	std::vector<std::unique_ptr<StatAST>> Stats = this->getStats();
	for (int i = 0; i < Stats.size(); i++) {
		if (Value* RetVal = Stats.at(i)->codegen()) {
			return RetVal;
		}
		else {
			return false;
		}
	}
	return Builder.CreateFAdd(llvm::make_unique<NumberExprAST>(0)->codegen(), llvm::make_unique<NumberExprAST>(0)->codegen(), "%success");

}

//TODO:
Value * PrintStatAST::codegen()
{
	return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

static void HandleFuncDefinition() {
	if (auto FnAST = ParseFuncDefinition()) {
		if (auto *FnIR = FnAST->codegen()) {
			fprintf(stderr, "Read function definition:");
			FnIR->print(errs());
			fprintf(stderr, "\n");
		}
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}

static void HandleFUNC() {
	if (ParseFUNC()) {
		fprintf(stderr, "Parsed an FUNC\n");
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}

static void HandleTopLevelExpression() {
	// Evaluate a top-level expression into an anonymous function.
	if (auto FnAST = ParseTopLevelExpr()) {
		if (auto *FnIR = FnAST->codegen()) {
			fprintf(stderr, "Read top-level expression:");
			FnIR->print(errs());
			fprintf(stderr, "\n");
		}
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}

/// top ::= definition | external | expression | ';'
static void MainLoop() {
	while (true) {
		fprintf(stderr, "ready> ");
		switch (CurTok) {
		case TOK_EOF:
			return;
		case ';': // ignore top-level semicolons.
			getNextToken();
			break;
		case FUNC:
			HandleFuncDefinition();
			break;
		default:
			HandleTopLevelExpression();
			break;
		}

	}
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
	// Install standard binary operators.
	// 1 is lowest precedence.
	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40; // highest.

							   // Prime the first token.
	fprintf(stderr, "ready> ");
	getNextToken();

	// Make the module, which holds all the code.
	TheModule = llvm::make_unique<Module>("my cool jit", TheContext);

	// Run the main "interpreter loop" now.
	MainLoop();

	// Print out all of the generated code.
	TheModule->print(errs(), nullptr);

	return 0;
}


//#include "llvm/ADT/APFloat.h"
//#include "llvm/ADT/STLExtras.h"
//#include "llvm/IR/BasicBlock.h"
//#include "llvm/IR/Constants.h"
//#include "llvm/IR/DerivedTypes.h"
//#include "llvm/IR/Function.h"
//#include "llvm/IR/IRBuilder.h"
//#include "llvm/IR/LLVMContext.h"
//#include "llvm/IR/Module.h"
//#include "llvm/IR/Type.h"
//#include "llvm/IR/Verifier.h"
//#include <algorithm>
//#include <cctype>
//#include <cstdio>
//#include <cstdlib>
//#include <map>
//#include <memory>
//#include <string>
//#include <vector>
//
//using namespace llvm;
//
////===----------------------------------------------------------------------===//
//// Lexer
////===----------------------------------------------------------------------===//
//
//// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
//// of these for known things.
//enum Token {
//  tok_eof = -1,
//
//  // commands
//  tok_def = -2,
//  tok_extern = -3,
//
//  // primary
//  tok_identifier = -4,
//  tok_number = -5
//};
//
//static std::string IdentifierStr; // Filled in if tok_identifier
//static double NumVal;             // Filled in if tok_number
//
///// gettok - Return the next token from standard input.
//static int gettok() {
//  static int LastChar = ' ';
//
//  // Skip any whitespace.
//  while (isspace(LastChar))
//    LastChar = getchar();
//
//  if (isalpha(LastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
//    IdentifierStr = LastChar;
//    while (isalnum((LastChar = getchar())))
//      IdentifierStr += LastChar;
//
//    if (IdentifierStr == "def")
//      return tok_def;
//    if (IdentifierStr == "extern")
//      return tok_extern;
//    return tok_identifier;
//  }
//
//  if (isdigit(LastChar) || LastChar == '.') { // Number: [0-9.]+
//    std::string NumStr;
//    do {
//      NumStr += LastChar;
//      LastChar = getchar();
//    } while (isdigit(LastChar) || LastChar == '.');
//
//    NumVal = strtod(NumStr.c_str(), nullptr);
//    return tok_number;
//  }
//
//  if (LastChar == '#') {
//    // Comment until end of line.
//    do
//      LastChar = getchar();
//    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
//
//    if (LastChar != EOF)
//      return gettok();
//  }
//
//  // Check for end of file.  Don't eat the EOF.
//  if (LastChar == EOF)
//    return tok_eof;
//
//  // Otherwise, just return the character as its ascii value.
//  int ThisChar = LastChar;
//  LastChar = getchar();
//  return ThisChar;
//}
//
////===----------------------------------------------------------------------===//
//// Abstract Syntax Tree (aka Parse Tree)
////===----------------------------------------------------------------------===//
//
//namespace {
//
///// ExprAST - Base class for all expression nodes.
//class ExprAST {
//public:
//  virtual ~ExprAST() = default;
//
//  virtual Value *codegen() = 0;
//};
//
///// NumberExprAST - Expression class for numeric literals like "1.0".
//class NumberExprAST : public ExprAST {
//  double Val;
//
//public:
//  NumberExprAST(double Val) : Val(Val) {}
//
//  Value *codegen() override;
//};
//
///// VariableExprAST - Expression class for referencing a variable, like "a".
//class VariableExprAST : public ExprAST {
//  std::string Name;
//
//public:
//  VariableExprAST(const std::string &Name) : Name(Name) {}
//
//  Value *codegen() override;
//};
//
///// BinaryExprAST - Expression class for a binary operator.
//class BinaryExprAST : public ExprAST {
//  char Op;
//  std::unique_ptr<ExprAST> LHS, RHS;
//
//public:
//  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS,
//                std::unique_ptr<ExprAST> RHS)
//      : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
//
//  Value *codegen() override;
//};
//
///// CallExprAST - Expression class for function calls.
//class CallExprAST : public ExprAST {
//  std::string Callee;
//  std::vector<std::unique_ptr<ExprAST>> Args;
//
//public:
//  CallExprAST(const std::string &Callee,
//              std::vector<std::unique_ptr<ExprAST>> Args)
//      : Callee(Callee), Args(std::move(Args)) {}
//
//  Value *codegen() override;
//};
//
///// PrototypeAST - This class represents the "prototype" for a function,
///// which captures its name, and its argument names (thus implicitly the number
///// of arguments the function takes).
//class PrototypeAST {
//  std::string Name;
//  std::vector<std::string> Args;
//
//public:
//  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
//      : Name(Name), Args(std::move(Args)) {}
//
//  Function *codegen();
//  const std::string &getName() const { return Name; }
//};
//
///// FunctionAST - This class represents a function definition itself.
//class FunctionAST {
//  std::unique_ptr<PrototypeAST> Proto;
//  std::unique_ptr<ExprAST> Body;
//
//public:
//  FunctionAST(std::unique_ptr<PrototypeAST> Proto,
//              std::unique_ptr<ExprAST> Body)
//      : Proto(std::move(Proto)), Body(std::move(Body)) {}
//
//  Function *codegen();
//};
//
//} // end anonymous namespace
//
////===----------------------------------------------------------------------===//
//// Parser
////===----------------------------------------------------------------------===//
//
///// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
///// token the parser is looking at.  getNextToken reads another token from the
///// lexer and updates CurTok with its results.
//static int CurTok;
//static int getNextToken() { return CurTok = gettok(); }
//
///// BinopPrecedence - This holds the precedence for each binary operator that is
///// defined.
//static std::map<char, int> BinopPrecedence;
//
///// GetTokPrecedence - Get the precedence of the pending binary operator token.
//static int GetTokPrecedence() {
//  if (!isascii(CurTok))
//    return -1;
//
//  // Make sure it's a declared binop.
//  int TokPrec = BinopPrecedence[CurTok];
//  if (TokPrec <= 0)
//    return -1;
//  return TokPrec;
//}
//
///// LogError* - These are little helper functions for error handling.
//std::unique_ptr<ExprAST> LogError(const char *Str) {
//  fprintf(stderr, "Error: %s\n", Str);
//  return nullptr;
//}
//
//std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
//  LogError(Str);
//  return nullptr;
//}
//
//static std::unique_ptr<ExprAST> ParseExpression();
//
///// numberexpr ::= number
//static std::unique_ptr<ExprAST> ParseNumberExpr() {
//  auto Result = llvm::make_unique<NumberExprAST>(NumVal);
//  getNextToken(); // consume the number
//  return std::move(Result);
//}
//
///// parenexpr ::= '(' expression ')'
//static std::unique_ptr<ExprAST> ParseParenExpr() {
//  getNextToken(); // eat (.
//  auto V = ParseExpression();
//  if (!V)
//    return nullptr;
//
//  if (CurTok != ')')
//    return LogError("expected ')'");
//  getNextToken(); // eat ).
//  return V;
//}
//
///// identifierexpr
/////   ::= identifier
/////   ::= identifier '(' expression* ')'
//static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
//  std::string IdName = IdentifierStr;
//
//  getNextToken(); // eat identifier.
//
//  if (CurTok != '(') // Simple variable ref.
//    return llvm::make_unique<VariableExprAST>(IdName);
//
//  // Call.
//  getNextToken(); // eat (
//  std::vector<std::unique_ptr<ExprAST>> Args;
//  if (CurTok != ')') {
//    while (true) {
//      if (auto Arg = ParseExpression())
//        Args.push_back(std::move(Arg));
//      else
//        return nullptr;
//
//      if (CurTok == ')')
//        break;
//
//      if (CurTok != ',')
//        return LogError("Expected ')' or ',' in argument list");
//      getNextToken();
//    }
//  }
//
//  // Eat the ')'.
//  getNextToken();
//
//  return llvm::make_unique<CallExprAST>(IdName, std::move(Args));
//}
//
///// primary
/////   ::= identifierexpr
/////   ::= numberexpr
/////   ::= parenexpr
//static std::unique_ptr<ExprAST> ParsePrimary() {
//  switch (CurTok) {
//  default:
//    return LogError("unknown token when expecting an expression");
//  case tok_identifier:
//    return ParseIdentifierExpr();
//  case tok_number:
//    return ParseNumberExpr();
//  case '(':
//    return ParseParenExpr();
//  }
//}
//
///// binoprhs
/////   ::= ('+' primary)*
//static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
//                                              std::unique_ptr<ExprAST> LHS) {
//  // If this is a binop, find its precedence.
//  while (true) {
//    int TokPrec = GetTokPrecedence();
//
//    // If this is a binop that binds at least as tightly as the current binop,
//    // consume it, otherwise we are done.
//    if (TokPrec < ExprPrec)
//      return LHS;
//
//    // Okay, we know this is a binop.
//    int BinOp = CurTok;
//    getNextToken(); // eat binop
//
//    // Parse the primary expression after the binary operator.
//    auto RHS = ParsePrimary();
//    if (!RHS)
//      return nullptr;
//
//    // If BinOp binds less tightly with RHS than the operator after RHS, let
//    // the pending operator take RHS as its LHS.
//    int NextPrec = GetTokPrecedence();
//    if (TokPrec < NextPrec) {
//      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
//      if (!RHS)
//        return nullptr;
//    }
//
//    // Merge LHS/RHS.
//    LHS =
//        llvm::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
//  }
//}
//
///// expression
/////   ::= primary binoprhs
/////
//static std::unique_ptr<ExprAST> ParseExpression() {
//  auto LHS = ParsePrimary();
//  if (!LHS)
//    return nullptr;
//
//  return ParseBinOpRHS(0, std::move(LHS));
//}
//
///// prototype
/////   ::= id '(' id* ')'
//static std::unique_ptr<PrototypeAST> ParsePrototype() {
//  if (CurTok != tok_identifier)
//    return LogErrorP("Expected function name in prototype");
//
//  std::string FnName = IdentifierStr;
//  getNextToken();
//
//  if (CurTok != '(')
//    return LogErrorP("Expected '(' in prototype");
//
//  std::vector<std::string> ArgNames;
//  while (getNextToken() == tok_identifier)
//    ArgNames.push_back(IdentifierStr);
//  if (CurTok != ')')
//    return LogErrorP("Expected ')' in prototype");
//
//  // success.
//  getNextToken(); // eat ')'.
//
//  return llvm::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
//}
//
///// definition ::= 'def' prototype expression
//static std::unique_ptr<FunctionAST> ParseDefinition() {
//  getNextToken(); // eat def.
//  auto Proto = ParsePrototype();
//  if (!Proto)
//    return nullptr;
//
//  if (auto E = ParseExpression())
//    return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
//  return nullptr;
//}
//
///// toplevelexpr ::= expression
//static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
//  if (auto E = ParseExpression()) {
//    // Make an anonymous proto.
//    auto Proto = llvm::make_unique<PrototypeAST>("__anon_expr",
//                                                 std::vector<std::string>());
//    return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
//  }
//  return nullptr;
//}
//
///// external ::= 'extern' prototype
//static std::unique_ptr<PrototypeAST> ParseExtern() {
//  getNextToken(); // eat extern.
//  return ParsePrototype();
//}
//
////===----------------------------------------------------------------------===//
//// Code Generation
////===----------------------------------------------------------------------===//
//
//static LLVMContext TheContext;
//static IRBuilder<> Builder(TheContext);
//static std::unique_ptr<Module> TheModule;
//static std::map<std::string, Value *> NamedValues;
//
//Value *LogErrorV(const char *Str) {
//  LogError(Str);
//  return nullptr;
//}
//
//Value *NumberExprAST::codegen() {
//  return ConstantFP::get(TheContext, APFloat(Val));
//}
//
//Value *VariableExprAST::codegen() {
//  // Look this variable up in the function.
//  Value *V = NamedValues[Name];
//  if (!V)
//    return LogErrorV("Unknown variable name");
//  return V;
//}
//
//Value *BinaryExprAST::codegen() {
//  Value *L = LHS->codegen();
//  Value *R = RHS->codegen();
//  if (!L || !R)
//    return nullptr;
//
//  switch (Op) {
//  case '+':
//    return Builder.CreateFAdd(L, R, "addtmp");
//  case '-':
//    return Builder.CreateFSub(L, R, "subtmp");
//  case '*':
//    return Builder.CreateFMul(L, R, "multmp");
//  case '<':
//    L = Builder.CreateFCmpULT(L, R, "cmptmp");
//    // Convert bool 0/1 to double 0.0 or 1.0
//    return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext), "booltmp");
//  default:
//    return LogErrorV("invalid binary operator");
//  }
//}
//
//Value *CallExprAST::codegen() {
//  // Look up the name in the global module table.
//  Function *CalleeF = TheModule->getFunction(Callee);
//  if (!CalleeF)
//    return LogErrorV("Unknown function referenced");
//
//  // If argument mismatch error.
//  if (CalleeF->arg_size() != Args.size())
//    return LogErrorV("Incorrect # arguments passed");
//
//  std::vector<Value *> ArgsV;
//  for (unsigned i = 0, e = Args.size(); i != e; ++i) {
//    ArgsV.push_back(Args[i]->codegen());
//    if (!ArgsV.back())
//      return nullptr;
//  }
//
//  return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
//}
//
//Function *PrototypeAST::codegen() {
//  // Make the function type:  double(double,double) etc.
//  std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(TheContext));
//  FunctionType *FT =
//      FunctionType::get(Type::getDoubleTy(TheContext), Doubles, false);
//
//  Function *F =
//      Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());
//
//  // Set names for all arguments.
//  unsigned Idx = 0;
//  for (auto &Arg : F->args())
//    Arg.setName(Args[Idx++]);
//
//  return F;
//}
//
//Function *FunctionAST::codegen() {
//  // First, check for an existing function from a previous 'extern' declaration.
//  Function *TheFunction = TheModule->getFunction(Proto->getName());
//
//  if (!TheFunction)
//    TheFunction = Proto->codegen();
//
//  if (!TheFunction)
//    return nullptr;
//
//  // Create a new basic block to start insertion into.
//  BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
//  Builder.SetInsertPoint(BB);
//
//  // Record the function arguments in the NamedValues map.
//  NamedValues.clear();
//  for (auto &Arg : TheFunction->args())
//    NamedValues[Arg.getName()] = &Arg;
//
//  if (Value *RetVal = Body->codegen()) {
//    // Finish off the function.
//    Builder.CreateRet(RetVal);
//
//    // Validate the generated code, checking for consistency.
//    verifyFunction(*TheFunction);
//
//    return TheFunction;
//  }
//
//  // Error reading body, remove function.
//  TheFunction->eraseFromParent();
//  return nullptr;
//}
//
////===----------------------------------------------------------------------===//
//// Top-Level parsing and JIT Driver
////===----------------------------------------------------------------------===//
//
//static void HandleDefinition() {
//  if (auto FnAST = ParseDefinition()) {
//    if (auto *FnIR = FnAST->codegen()) {
//      fprintf(stderr, "Read function definition:");
//      FnIR->print(errs());
//      fprintf(stderr, "\n");
//    }
//  } else {
//    // Skip token for error recovery.
//    getNextToken();
//  }
//}
//
//static void HandleExtern() {
//  if (auto ProtoAST = ParseExtern()) {
//    if (auto *FnIR = ProtoAST->codegen()) {
//      fprintf(stderr, "Read extern: ");
//      FnIR->print(errs());
//      fprintf(stderr, "\n");
//    }
//  } else {
//    // Skip token for error recovery.
//    getNextToken();
//  }
//}
//
//static void HandleTopLevelExpression() {
//  // Evaluate a top-level expression into an anonymous function.
//  if (auto FnAST = ParseTopLevelExpr()) {
//    if (auto *FnIR = FnAST->codegen()) {
//      fprintf(stderr, "Read top-level expression:");
//      FnIR->print(errs());
//      fprintf(stderr, "\n");
//    }
//  } else {
//    // Skip token for error recovery.
//    getNextToken();
//  }
//}
//
///// top ::= definition | external | expression | ';'
//static void MainLoop() {
//  while (true) {
//    fprintf(stderr, "ready> ");
//    switch (CurTok) {
//    case tok_eof:
//      return;
//    case ';': // ignore top-level semicolons.
//      getNextToken();
//      break;
//    case tok_def:
//      HandleDefinition();
//      break;
//    case tok_extern:
//      HandleExtern();
//      break;
//    default:
//      HandleTopLevelExpression();
//      break;
//    }
//  }
//}
//
////===----------------------------------------------------------------------===//
//// Main driver code.
////===----------------------------------------------------------------------===//
//
//int main() {
//  // Install standard binary operators.
//  // 1 is lowest precedence.
//  BinopPrecedence['<'] = 10;
//  BinopPrecedence['+'] = 20;
//  BinopPrecedence['-'] = 20;
//  BinopPrecedence['*'] = 40; // highest.
//
//  // Prime the first token.
//  fprintf(stderr, "ready> ");
//  getNextToken();
//
//  // Make the module, which holds all the code.
//  TheModule = llvm::make_unique<Module>("my cool jit", TheContext);
//
//  // Run the main "interpreter loop" now.
//  MainLoop();
//
//  // Print out all of the generated code.
//  TheModule->print(errs(), nullptr);
//
//  return 0;
//}