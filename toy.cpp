#include "../include/KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
<<<<<<< HEAD
#include "llvm/Transforms/InstCombine/InstCombine.h"
=======
#include "llvm/Support/TargetRegistry.h"
>>>>>>> 16b48486c59db9f7ab4287b3b29c36cabb7752ab
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
<<<<<<< HEAD

=======
using namespace llvm::orc;
>>>>>>> 16b48486c59db9f7ab4287b3b29c36cabb7752ab
//===----------------------------------------------------------------------===//
// Lexer
// 词法分析器部分
//===----------------------------------------------------------------------===//

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
// 词法分析器  如果是已知字符，返回对应token
//				否则返回[0-255]（ascii码）
enum Token {
	TOK_EOF = -1,
	ERROR = -2,

	//变量、整数、文字、赋值符号:=
	VARIABLE = -3,
	INTEGER = -4,
	TEXT = -5,
	ASSIGN_SYMBOL = -6,

	// commands
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

static std::string IdentifierStr; //存储变量名 Filled in if VARIABLE
static int NumVal;                //存储数字 Filled in if INTERGER
static std::string TextStr;       //存储text Filled in if TEXT
static std::vector<std::string> cifa;
static std::vector<std::string> yufa;
static std::string space;
								  // gettok - Return the next token from standard input.
								  // gettok - 从标准输入中获取下一个字符
static int gettok() {
	static int LastChar = ' ';

	// Skip any whitespace.跳过空格
	while (isspace(LastChar))
		LastChar = getchar();

	// Commands and VARIABLE 变量声明符号和已知命令符号
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

		if (IdentifierStr == "FUNC") {
			cifa.push_back("FUNC--tok_FUNC");
			return FUNC;
		}
		if (IdentifierStr == "PRINT") {
			cifa.push_back("PRINT--tok_PRINT");
			return PRINT;
		}
		if (IdentifierStr == "RETURN") {
			cifa.push_back("RETURN--tok_RETURN");
			return RETURN;
		}
		if (IdentifierStr == "CONTINUE") {
			cifa.push_back("CONTINUE--tok_CONTINUE");
			return CONTINUE;
		}
		if (IdentifierStr == "IF") {
			cifa.push_back("IF--tok_IF");
			return IF;
		}
		if (IdentifierStr == "THEN") {
			cifa.push_back("THEN--tok_THEN");
			return THEN;
		}
		if (IdentifierStr == "ELSE") {
			cifa.push_back("ELSE--tok_ELSE");
			return ELSE;
		}
		if (IdentifierStr == "FI") {
			cifa.push_back("FI--tok_FI");
			return FI;
		}
		if (IdentifierStr == "WHILE") {
			cifa.push_back("WHILE--tok_WHILE");
			return WHILE;
		}
		if (IdentifierStr == "DO") {
			cifa.push_back("DO--tok_DO");
			return DO;
		}
		if (IdentifierStr == "DONE") {
			cifa.push_back("DONE--tok_DONE");
			return DONE;
		}
		if (IdentifierStr == "VAR") {
			cifa.push_back("VAR--tok_VAR");
			return VAR;
		}
	}

	// INTERGER 整数
	if (isdigit(LastChar)) {
		std::string NumStr;
		do {
			NumStr += LastChar;
			LastChar = getchar();
		} while (isdigit(LastChar));
		cifa.push_back(NumStr + "--tok_number");
		NumVal = atoi(NumStr.c_str());
		return INTEGER;
	}

	
	if (LastChar == '/') {
		LastChar = getchar();
		if (LastChar == '/') {
			do {
				LastChar = getchar();
			} while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
			cifa.push_back("//****--tok_command");
			if (LastChar != EOF)
				return gettok();
		}
		return ERROR;
	}

	// ASSIGN_SYMBOL 赋值符号
	if (LastChar == ':') {
		LastChar = getchar();
		if (LastChar == '=') {
			LastChar = getchar();
			cifa.push_back(":=--tok_ASSIGN");
			return ASSIGN_SYMBOL;
		}
		return ERROR;
	}
	// Check for end of file.  Don't eat the EOF. 检查是否为文件尾
	// TEXT 文本
	if (LastChar == '"') {
		IdentifierStr = "";
		while ((LastChar = getchar()) != '"') {
			IdentifierStr += LastChar;
		}
		LastChar = getchar();
		cifa.push_back(IdentifierStr + "--tok_text");
		return TEXT;
	}
	if (LastChar == EOF)
		return TOK_EOF;

	// Otherwise, just return the character as its ascii value.
	// 未知符号返回其ascii码值
	int ThisChar = LastChar;
	int i = ThisChar;
	LastChar = getchar();
	cifa.push_back(std::to_string(i) + "--tok_of_ascii");
	return ThisChar;
}

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
// 抽象语法树
//===----------------------------------------------------------------------===//

namespace {

	/// ExprAST - Base class for all expression nodes.
	class ExprAST {
	public:
		virtual ~ExprAST() = default;
		virtual Value *codegen() = 0;
	};


	/// StatAST - This class for statement
	class StatAST : public ExprAST {
	public:
		virtual ~StatAST() = default;
		virtual Value *codegen() = 0;
		virtual bool isRet();
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
		std::vector<std::unique_ptr<ExprAST>> Args;
	public:
		CallExprAST(const std::string &Callee,
			std::vector<std::unique_ptr<ExprAST>> Args)
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
		std::unique_ptr<ExprAST> Body;
	public:
		FunctionAST(std::unique_ptr<PrototypeAST> Proto,
			std::unique_ptr<ExprAST> Body)
			: Proto(std::move(Proto)), Body(std::move(Body)) {}
		Function *codegen();
	};


	/// ProgAST - the program没用到
	class ProgAST {
		std::vector<std::unique_ptr<FunctionAST>> Funs;

	public:
		ProgAST(std::vector<std::unique_ptr<FunctionAST>> Funs)
			: Funs(std::move(Funs)) {}
	};


	/// DeclAST - This class for declaration
	class DeclExprAST : public StatAST {
		std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
	public:
		DeclExprAST(
			std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames)
			: VarNames(std::move(VarNames)) {}
		Value *codegen() override;
	};


	/// NullStatAST-Statement class for null statement
	class NullStatAST : public StatAST {
	public:
		NullStatAST() {}
		Value *codegen() override;
	};


	/// ReturnStatAST - Statement class for return statement
	class ReturnStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;
	public:
		ReturnStatAST(std::unique_ptr<ExprAST> Exp) : Exp(std::move(Exp)) {}
		bool isRet() { return true; }
		Value *codegen() override;
	};


	/// AssignStatAST - Statement class for Assignment Statement
	class AssignStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp;
		std::string Name;
	public:
		AssignStatAST(const std::string &Name, std::unique_ptr<ExprAST> Exp)
			: Name(Name), Exp(std::move(Exp)) {}
		Value *codegen() override;
	};


	/// IfStatAST - Statement class for IF Statement
	class IfStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp, tStat, eStat;
	public:
		IfStatAST(std::unique_ptr<ExprAST> Exp, std::unique_ptr<ExprAST> tStat)
			: Exp(std::move(Exp)), tStat(std::move(tStat)) {
			eStat = nullptr;
		}
		IfStatAST(std::unique_ptr<ExprAST> Exp, std::unique_ptr<ExprAST> tStat,
			std::unique_ptr<ExprAST> eStat)
			: Exp(std::move(Exp)), tStat(std::move(tStat)), eStat(std::move(eStat)) {}
		Value *codegen() override;
	};


	/// WhileStatAST - Statement class for WHILE Statement
	class WhileStatAST : public StatAST {
		std::unique_ptr<ExprAST> Exp, Stat;
	public:
		WhileStatAST(std::unique_ptr<ExprAST> Exp, std::unique_ptr<ExprAST> Stat)
			: Exp(std::move(Exp)), Stat(std::move(Stat)) {}
		Value *codegen() override;
	};


	/// BlockStatAST - Statement class for Block Statement "{……}"
	class BlockAST : public StatAST {
		std::vector<std::unique_ptr<DeclExprAST>> Decls;
		std::vector<std::unique_ptr<StatAST>> Stats;
	public:
		std::vector<std::unique_ptr<StatAST>> getStats() { return std::move(Stats); }
		std::vector<std::unique_ptr<DeclExprAST>> getDecls() {
			return std::move(Decls);
		}
		BlockAST(std::vector<std::unique_ptr<DeclExprAST>> Decls,
			std::vector<std::unique_ptr<StatAST>> Stats)
			: Decls(std::move(Decls)), Stats(std::move(Stats)) {}
		bool r = false;
		bool isRet() { return r; }
		Value *codegen() override;
	};


	/// PrintItemAST - class for Print Item Statement 两种不同打印项的基类
	class PrintItemAST {
	public:
		static const int type;
		virtual ~PrintItemAST() = default;
		virtual Value *codegen() = 0;
	};


	/// ExpPrintItemAST - Print Item class for Expression Print Item 表达式类打印项
	class ExpPrintItemAST : public PrintItemAST {
		std::unique_ptr<ExprAST> Exp;
		static const int type = 0;
	public:
		ExpPrintItemAST(std::unique_ptr<ExprAST> Exp) : Exp(std::move(Exp)) {}
		Value *codegen() override;
	};


	/// TextPrintItemAST - Print Item class for TEXT Print Item 文本类打印项
	class TextPrintItemAST : public PrintItemAST {
		static const int type = 1;
	public:
		std::string Text;
		TextPrintItemAST(std::string Text) : Text(Text) {}
		Value *codegen() override;
	};


	/// PrintStatAST - Statement class for  Print Item Statement
	class PrintStatAST : public StatAST {
		std::vector<std::unique_ptr<PrintItemAST>> Items;
	public:
		PrintStatAST(std::vector<std::unique_ptr<PrintItemAST>> Items)
			: Items(std::move(Items)) {}
		Value *codegen() override;
	};

} // end anonymous namespace

  //===----------------------------------------------------------------------===//
  // Parser
  // 语法分析器
  //===----------------------------------------------------------------------===//

  /// CurTok/getNextToken - Provide a simple token buffer.  CurTok is the current
  /// token the parser is looking at.  getNextToken reads another token from the
  /// lexer and updates CurTok with its results.
static int CurTok; //当前token
static int getNextToken() {
	return CurTok = gettok();
} //获取下一个token到当前token


  /// BinopPrecedence - This holds the precedence for each binary operator that is
  /// defined.	二元运算符优先级
static std::map<char, int> BinopPrecedence;


/// GetTokPrecedence - Get the precedence of the pending binary operator token.
//  获取token的优先级
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
/// 输出错误
std::unique_ptr<ExprAST> LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
	LogError(Str);
	return nullptr;
}


// 提前声明
static std::unique_ptr<ExprAST> ParseExpression();


///数字 numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr() {
	yufa.push_back(space + "Number_Statement:\n");
	auto Result = llvm::make_unique<NumberExprAST>(NumVal);
	getNextToken(); // consume the number
	return std::move(Result);
}


///（表达式） parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
	yufa.push_back(space + "ParenExpr_Statement:\n");
	space += " ";

	yufa.push_back(space);
	getNextToken(); // eat (.
	auto V = ParseExpression();
	if (!V)
		return nullptr;

	if (CurTok != ')')
		return LogError("expected ')'");
	getNextToken(); // eat ).
	space = space.substr(0, space.length() - 1);
	return V;
}


/// 变量表达式 VARIABLE | VARIABLE '(' argument_list ')'
///	identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseVariableExpr() {
	yufa.push_back(space + "Identifier_Statement:\n");
	space += " ";
	std::string IdName = IdentifierStr;

	getNextToken(); // eat identifier.

	if (CurTok != '(')                                   // Simple variable ref.
		return llvm::make_unique<VariableExprAST>(IdName); // VARIABLE

														   // Call.
	getNextToken(); // eat (
	std::vector<std::unique_ptr<ExprAST>> Args;
	if (CurTok != ')') {
		while (true) {
			yufa.push_back(space);
			if (auto Arg = ParseExpression())
				Args.push_back(std::move(Arg));
			else
				return nullptr;

			if (CurTok == ')')
				break;

			if (CurTok != ',')
				return LogError(
					"Expected ')' or ',' in argument list-参数列表缺少）或，");
			getNextToken();
		}
	}

	// Eat the ')'.
	getNextToken();
	space = space.substr(0, space.length() - 1);
	return llvm::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// expression with '-'
static std::unique_ptr<ExprAST> ParseNegaExpr() {
	//getNextToken(); // eat -.
	//auto V = ParseExpression();
	//if (!V)
	//	return nullptr;

	//return V;
	yufa.push_back(space + "NegaExpr_Statement:\n");
	space += " ";
	auto Result = llvm::make_unique<NumberExprAST>(0);
	int FuTok = CurTok;
	getNextToken();
	switch (CurTok) {
	default:
		return LogError("unknown token when expecting an expression");
	case VAR:
		yufa.push_back(space);
		space = space.substr(0, space.length() - 1);
		return llvm::make_unique<BinaryExprAST>(FuTok, std::move(Result),
			std::move(ParseVariableExpr()));
	case INTEGER:
		yufa.push_back(space);
		space = space.substr(0, space.length() - 1);
		return llvm::make_unique<BinaryExprAST>(FuTok, std::move(Result),
			std::move(ParseNumberExpr()));
	case '(':
		yufa.push_back(space);
		space = space.substr(0, space.length() - 1);
		return llvm::make_unique<BinaryExprAST>(FuTok, std::move(Result),
			std::move(ParseParenExpr()));
	}
}


static std::unique_ptr<StatAST> ParseStatment();

/// Decl: VAR variable_list
static std::unique_ptr<DeclExprAST> ParseDecl() {
	if (CurTok != VAR)
		return nullptr;
	std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
	do {
		if (getNextToken() == VARIABLE) {
			std::string var = IdentifierStr;
			std::unique_ptr<ExprAST> Init = llvm::make_unique<NumberExprAST>(0);
			VarNames.push_back(std::make_pair(var, std::move(Init)));
			getNextToken();
		}
		else {
			LogError("Expected VARIABLE after VAR");
			return nullptr;
		}
	} while (CurTok == ',');
	return llvm::make_unique<DeclExprAST>(std::move(VarNames));
}

/// primary
///   ::= VariableExpr
///   ::= NumberExpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
	yufa.push_back(space + "Primary_Statement:\n");
	space += " ";
	switch (CurTok) {
	default:
		return ParseStatment();
	case VARIABLE:
		yufa.push_back(space);
		space = space.substr(0, space.length() - 1);
		return ParseVariableExpr();
	case INTEGER:
		yufa.push_back(space);
		space = space.substr(0, space.length() - 1);
		return ParseNumberExpr();
	case '(':
		yufa.push_back(space);
		space = space.substr(0, space.length() - 1);
		return ParseParenExpr();
	case '-':
		yufa.push_back(space);
		space = space.substr(0, space.length() - 1);
		return ParseNegaExpr();
	}
}

/// binoprhs
///   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
	std::unique_ptr<ExprAST> LHS) {
	yufa.push_back(space + "BinOpRHS_Statement:\n");
	space += " ";
	// If this is a binop, find its precedence.
	while (true) {
		int TokPrec = GetTokPrecedence();

		// If this is a binop that binds at least as tightly as the current binop,
		// consume it, otherwise we are done.
		if (TokPrec < ExprPrec) {
			space = space.substr(0, space.length() - 1);
			return LHS;
		}
		// Okay, we know this is a binop.
		int BinOp = CurTok;
		getNextToken(); // eat binop
		yufa.push_back(space);
						// Parse the primary expression after the binary operator.
		auto RHS = ParsePrimary();
		if (!RHS)
			return nullptr;

		// If BinOp binds less tightly with RHS than the operator after RHS, let
		// the pending operator take RHS as its LHS.
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			yufa.push_back(space);
			RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
			if (!RHS)
				return nullptr;
		}

		// Merge LHS/RHS.
		LHS =
			llvm::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
	}
}

/// expression
///   ::= primary binoprhs
/// 表达式
static std::unique_ptr<ExprAST> ParseExpression() {
	yufa.push_back(space + "Expression_Statement:\n");
	space += " ";
	yufa.push_back(space);
	auto LHS = ParsePrimary();
	if (!LHS)
		return nullptr;
	yufa.push_back(space);
	space = space.substr(0, space.length() - 1);
	return ParseBinOpRHS(0, std::move(LHS));
}

/// AssignStat:  VARIABLE := expression
// 赋值语句
static std::unique_ptr<StatAST> ParseAssignStat() {
	yufa.push_back(space + "Assignment_Statement:\n");
	space += " ";
	std::string Vname = IdentifierStr; //得到变量名
	getNextToken();                    // eat variable name
	if (CurTok != ASSIGN_SYMBOL) {
		LogError("缺少赋值符号:=  Expected ':=' in prototype");
		return nullptr;
	}
	getNextToken();
	if (auto E = ParseExpression()) {
		yufa.push_back(space);
		space = space.substr(0, space.length() - 1);
		return llvm::make_unique<AssignStatAST>(Vname, std::move(E));
	}
	return nullptr;
}

/// PrintStat:	PRINT "text"/expression
// print语句
static std::unique_ptr<StatAST> ParsePrintStat() {
	std::vector<std::unique_ptr<PrintItemAST>> Items; //存储要打印的项集
	do {
		if (getNextToken() == TEXT) {
			Items.push_back(llvm::make_unique<TextPrintItemAST>(IdentifierStr));
			getNextToken();
		}
		else {
			Items.push_back(llvm::make_unique<ExpPrintItemAST>(ParseExpression()));
		}
	} while (CurTok == ',');
	return llvm::make_unique<PrintStatAST>(std::move(Items));
}


// ReturnStat:	RETURN expression
// return语句
static std::unique_ptr<StatAST> ParseReturnStat() {
	yufa.push_back(space + "Return_Statement:\n");
	space += " ";
	getNextToken(); // eat RETURN
	if (auto E = ParseExpression()) {
		yufa.push_back(space);
		space = space.substr(0, space.length() - 1);
		return llvm::make_unique<ReturnStatAST>(std::move(E));
	}
	return nullptr;
}

// NullStat:		CONTINUE
// continue 语句
static std::unique_ptr<StatAST> ParseNullStat() {
	yufa.push_back(space + "Continue_Statement:\n");
	space += " ";
	getNextToken(); // eat CONTINUE
	space = space.substr(0, space.length() - 1);
	return llvm::make_unique<NullStatAST>();
}

/// IfStat:  IF expression THEN statement FI
///			| IF expression THEN statement
///			  ELSE statement FI
static std::unique_ptr<StatAST> ParseIfStat() {
	yufa.push_back(space + "If_Statement:\n");
	space += " ";
	getNextToken(); // eat IF
	yufa.push_back(space);
	if (auto E = ParseExpression()) {
		if (CurTok != THEN) {
			LogError("Expected 'THEN'");
			return nullptr;
		}
		getNextToken(); // eat THEN
		yufa.push_back(space);
		if (auto tS = ParseStatment()) {
			// getNextToken();
			if (CurTok == FI) {
				getNextToken();
				space = space.substr(0, space.length() - 1);
				return llvm::make_unique<IfStatAST>(std::move(E), std::move(tS));
			}
			else if (CurTok == ELSE) {
				getNextToken();
				yufa.push_back(space);
				if (auto eS = ParseStatment()) {
					if (CurTok == FI) {
						getNextToken();
						space = space.substr(0, space.length() - 1);
						return llvm::make_unique<IfStatAST>(std::move(E), std::move(tS),
							std::move(eS));
					}
					else {
						LogError("Expected 'FI'");
						return nullptr;
					}
				}
				else {
					LogError("Expected Statement");
					return nullptr;
				}
			}
			else {
				LogError("Expected 'ELSE' or 'FI'");
				return nullptr;
			}
		}
		return nullptr;
	}
	return nullptr;
}

/// WhileStat:  WHILE expression DO statement DONE
static std::unique_ptr<StatAST> ParseWhileStat() {
	yufa.push_back(space + "While_Statement:\n");
	space += " ";
	getNextToken(); // eat WHILE
	yufa.push_back(space);
	if (auto E = ParseExpression()) {
		if (CurTok != DO) {
			LogError("缺少‘DO’ Expected 'DO'");
			return nullptr;
		}
		getNextToken(); // eat DO
		yufa.push_back(space);
		if (auto tS = ParseStatment()) {
			if (CurTok == DONE) {
				getNextToken();
				space = space.substr(0, space.length() - 1);
				return llvm::make_unique<WhileStatAST>(std::move(E), std::move(tS));
			}
			else {
				LogError("Expected 'DONE'");
				return nullptr;
			}
		}
		return nullptr;
	}
	return nullptr;
}

/// BlockStat:	'{' declaration_list statement_list '}'
static std::unique_ptr<StatAST> ParseBlockStat() {
	yufa.push_back(space + "Block_Statement:\n");
	space += " ";
	getNextToken(); // eat '{'
	std::vector<std::unique_ptr<DeclExprAST>> Decls;
	std::vector<std::unique_ptr<StatAST>> Stats;
	int i = 0;
	while (CurTok != '}' && i == 0) {
		if (auto D = ParseDecl()) {
			yufa.push_back(space);
			Decls.push_back(std::move(D));
		}
		else {
			i++;
		}
	}
	while (CurTok != '}' && i == 1) {
		if (auto S = ParseStatment()) {
			yufa.push_back(space);
			Stats.push_back(std::move(S));
		}
		else {
			i++;
		}
	}
	if (CurTok == '}') {
		getNextToken();
		space = space.substr(0, space.length() - 1);
		return llvm::make_unique<BlockAST>(std::move(Decls), std::move(Stats));
	}
	else {
		LogError("Expected '}'");
		return nullptr;
	}
}

///Statement
static std::unique_ptr<StatAST> ParseStatment() {
	yufa.push_back(space + "Statement_Statement:\n");
	space += " ";
	switch (CurTok) {
	case VAR:
		yufa.push_back(space);
		return ParseDecl();
	case VARIABLE:
		yufa.push_back(space);
		return ParseAssignStat();
	case PRINT:
		yufa.push_back(space);
		return ParsePrintStat();
	case RETURN:
		yufa.push_back(space);
		return ParseReturnStat();
	case CONTINUE:
		yufa.push_back(space);
		return ParseNullStat();
	case IF:
		yufa.push_back(space);
		return ParseIfStat();
	case WHILE:
		yufa.push_back(space);
		return ParseWhileStat();
	case '{':
		yufa.push_back(space);
		return ParseBlockStat();
	default:
		break;
	}
	return nullptr;
}

/// prototype
///   ::= VARIABLE '(' VARIABLE_LIST ')'
static std::unique_ptr<PrototypeAST> ParsePrototype() {
	yufa.push_back(space + "Prototype_Statement:\n");
	if (CurTok != VARIABLE)
		return LogErrorP("Expected function name in prototype");

	std::string FnName = IdentifierStr; //得到函数名
	getNextToken();                     // eat function name

	if (CurTok != '(')
		return LogErrorP("Expected '(' in prototype");

	std::vector<std::string> ArgNames;
	if (getNextToken() == VARIABLE) {
		ArgNames.push_back(IdentifierStr);
		while (getNextToken() == ',') {
			getNextToken(); // eat ','
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
	yufa.push_back(space + "FuncDefinition_Statement:\n");
	space += " ";
	yufa.push_back(space);
	getNextToken(); // eat FUNC.
	auto Proto = ParsePrototype();
	if (!Proto) {
		space = space.substr(0, space.length() - 1);
		return nullptr;
	}
	yufa.push_back(space);
	if (auto E = ParseStatment()) {
		space = space.substr(0, space.length() - 1);
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	}
	space = space.substr(0, space.length() - 1);
	return nullptr;
}

/// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
	if (auto E = ParseExpression()) {
		// Make an anonymous proto.
		auto Proto = llvm::make_unique<PrototypeAST>("__anon_expr",
			std::vector<std::string>());
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	}
	return nullptr;
}



//===----------------------------------------------------------------------===//
// Code Generation
// 中间代码生成
//===----------------------------------------------------------------------===//

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::unique_ptr<legacy::FunctionPassManager> TheFPM;
static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static std::map<std::string, AllocaInst *> NamedValues;

Value *LogErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
	const std::string &VarName) {
	IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
		TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(Type::getInt32Ty(TheContext), nullptr, VarName.c_str());
}

//获得已知FUNCTION
Function *getFunction(std::string Name) {
	// First, see if the function has already been added to the current module.
	if (auto *F = TheModule->getFunction(Name))
		return F;

	// If not, check whether we can codegen the declaration from some existing
	// prototype.
	auto FI = FunctionProtos.find(Name);
	if (FI != FunctionProtos.end())
		return FI->second->codegen();

	// If no existing prototype exists, return null.
	return nullptr;
}

/// VAR VARIABLE变量声明
Value *DeclExprAST::codegen() {
	std::vector<AllocaInst *> OldBindings;

	Function *TheFunction = Builder.GetInsertBlock()->getParent();
	// Register all variables and emit their initializer.

	for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
		const std::string &VarName = VarNames[i].first;
		ExprAST *Init = VarNames[i].second.get();

		Value *InitVal;
		if (Init) {
			InitVal = Init->codegen();
			if (!InitVal)
				return nullptr;
		}

		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
		Builder.CreateStore(InitVal, Alloca);
		// Remember the old variable binding so that we can restore the binding when
		// we unrecurse.
		OldBindings.push_back(NamedValues[VarName]);
		// Remember this binding.
		NamedValues[VarName] = Alloca;
	}

	return ConstantInt::get(TheContext, APInt(32, 0, true));
}

Value *NumberExprAST::codegen() {
	return ConstantInt::get(TheContext, APInt(32, Val, true));
}

Value *VariableExprAST::codegen() {
	// Look this variable up in the function.
	Value *V = NamedValues[Name];
	if (!V)
		return LogErrorV("Unknown variable name");
	return Builder.CreateLoad(V, Name.c_str());
}

Value *BinaryExprAST::codegen() {
	Value *L = LHS->codegen();
	Value *R = RHS->codegen();
	if (!L || !R)
		return nullptr;

	switch (Op) {
	case '+':
		return Builder.CreateAdd(L, R, "addtmp");
	case '-':
		return Builder.CreateSub(L, R, "subtmp");
	case '*':
		return Builder.CreateMul(L, R, "multmp");
	case '/':
		return Builder.CreateExactSDiv(L, R, "divtmp");
		// Convert bool 0/1 to double 0.0 or 1.0
		//return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext), "booltmp");
	default:
		return LogErrorV("invalid binary operator");
	}
}

Value *CallExprAST::codegen() {
	// Look up the name in the global module table.
	Function *CalleeF = getFunction(Callee);
	if (!CalleeF)
		return LogErrorV("Unknown function referenced");

	// If argument mismatch error.
	if (CalleeF->arg_size() != Args.size())
		return LogErrorV("Incorrect # arguments passed");

	std::vector<Value *> ArgsV;
	for (unsigned i = 0, e = Args.size(); i != e; ++i) {
		ArgsV.push_back(Args[i]->codegen());
		if (!ArgsV.back())
			return nullptr;
	}
	Value *mm = Builder.CreateCall(CalleeF, ArgsV, "calltmp");
	return mm;
}

Function *PrototypeAST::codegen() {
<<<<<<< HEAD
	// Make the function type:  double(double,double) etc.
	std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(TheContext));
	FunctionType *FT =
		FunctionType::get(Type::getDoubleTy(TheContext), Doubles, false);
=======


	// Make the function type:  int64(int64,int64) etc.
	std::vector<Type *> Ints(Args.size(), Type::getInt32Ty(TheContext));
	//std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(TheContext));

	FunctionType *FT =
		FunctionType::get(Type::getInt32Ty(TheContext), Ints, false);
	/*FunctionType *FT =
	FunctionType::get(Type::getDoubleTy(TheContext), Ints, false);*/
>>>>>>> 16b48486c59db9f7ab4287b3b29c36cabb7752ab

	Function *F =
		Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

	// Set names for all arguments.
	unsigned Idx = 0;
	for (auto &Arg : F->args())
		Arg.setName(Args[Idx++]);

	return F;
}

bool StatAST::isRet() { return false; }

Function *FunctionAST::codegen() {
	auto &P = *Proto;
	FunctionProtos[Proto->getName()] = std::move(Proto);
	Function *TheFunction = getFunction(P.getName());
	if (!TheFunction)
		return nullptr;

	// Create a new basic block to start insertion into.
	BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
	Builder.SetInsertPoint(BB);

	// Record the function arguments in the NamedValues map.
	NamedValues.clear();
	for (auto &Arg : TheFunction->args()) {
		// Create an alloca for this variable.
		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName());

		// Store the initial value into the alloca.
		Builder.CreateStore(std::move(&Arg), Alloca);

		// Add arguments to variable symbol table.
		NamedValues[Arg.getName()] = Alloca;
	}
	if (Value *RetVal = Body->codegen()) {
		// Finish off the function.
		// if (!Body->isRet())
		Builder.CreateRet(RetVal);

		// Validate the generated code, checking for consistency.
		verifyFunction(*TheFunction);

		// Run the optimizer on the function.
		//TheFPM->run(*TheFunction);

		return TheFunction;
<<<<<<< HEAD

		// Error reading body, remove function.
		TheFunction->eraseFromParent();
		return nullptr;
}
=======
	}
>>>>>>> 16b48486c59db9f7ab4287b3b29c36cabb7752ab

	// Error reading body, remove function.
	TheFunction->eraseFromParent();
	return nullptr;
}

Value *NullStatAST::codegen() {
	//如果是空声明则返回0;
	return ConstantInt::get(TheContext, APInt(32, 0, true));
}

Value *ReturnStatAST::codegen() {
	if (Value *RetVal = Exp->codegen()) {
		return RetVal;
	}
	return nullptr;
}

Value *AssignStatAST::codegen() {
	if (Value *RetVal = Exp->codegen()) {
		Value *Variable = NamedValues[Name];
		if (!Variable)
			return LogErrorV("Unknown variable name");

		Builder.CreateStore(RetVal, Variable);
		return RetVal;
	}
	return LogErrorV("Wrong Expression after ':='");
}

Value *IfStatAST::codegen() {
	Value *CondV = Exp->codegen();
	if (!CondV)
		return nullptr;

	// Convert condition to a bool by comparing non-equal to 0.0.
	CondV = Builder.CreateICmpNE(CondV, Builder.getInt32(0), "ifcond");

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	// Create blocks for the then and else cases.  Insert the 'then' block at the
	// end of the function.
	BasicBlock *ThenBB = BasicBlock::Create(TheContext, "then", TheFunction);
	BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else");
	BasicBlock *MergeBB = BasicBlock::Create(TheContext, "ifcont");

	Builder.CreateCondBr(CondV, ThenBB, ElseBB);

	// Emit then value.
	Builder.SetInsertPoint(ThenBB);

	Value *ThenV = tStat->codegen();
	if (!ThenV)
		return nullptr;

	Builder.CreateBr(MergeBB);
	// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
	ThenBB = Builder.GetInsertBlock();

	// Emit else block.
	TheFunction->getBasicBlockList().push_back(ElseBB);
	Builder.SetInsertPoint(ElseBB);
	Value *ElseV;
	if (eStat != nullptr) {
		ElseV = eStat->codegen();
		if (!ElseV)
			return nullptr;
	}

	Builder.CreateBr(MergeBB);
	// Codegen of 'Else' can change the current block, update ElseBB for the PHI.
	ElseBB = Builder.GetInsertBlock();

	// Emit merge block.
	TheFunction->getBasicBlockList().push_back(MergeBB);
	Builder.SetInsertPoint(MergeBB);

	PHINode *PN = Builder.CreatePHI(Type::getInt32Ty(TheContext), 2, "iftmp");

	PN->addIncoming(ThenV, ThenBB);

	if (eStat != nullptr) {
		PN->addIncoming(ElseV, ElseBB);
	}

	return PN;
}

Value * ::WhileStatAST::codegen() {
	Value *EndCond = Exp->codegen();
	if (!EndCond)
		return nullptr;

	EndCond = Builder.CreateICmpNE(EndCond, Builder.getInt32(0), "whilecond");

	Function *TheFunction = Builder.GetInsertBlock()->getParent();

	BasicBlock *PreheaderBB = Builder.GetInsertBlock();

	BasicBlock *LoopBB = BasicBlock::Create(TheContext, "loop", TheFunction);

	Builder.CreateBr(LoopBB);

	Builder.SetInsertPoint(LoopBB);

	//PHINode *Variable =
	//	Builder.CreatePHI(Type::getInt32Ty(TheContext), 2, "loopend");

	// Variable->addIncoming(EndCond, PreheaderBB);

	if (!Stat->codegen())
		return nullptr;

	EndCond = Exp->codegen();
	if (!EndCond)
		return nullptr;

	EndCond = Builder.CreateICmpNE(
		EndCond, Builder.getInt32(0), "whilecond");

	BasicBlock *LoopEndBB = Builder.GetInsertBlock();

	BasicBlock *AfterBB =
		BasicBlock::Create(TheContext, "afterloop", TheFunction);

	Builder.CreateCondBr(EndCond, LoopBB, AfterBB);

	Builder.SetInsertPoint(AfterBB);

	// Variable->addIncoming(EndCond, LoopEndBB);

	return Builder.getInt32(0);
}

Value *BlockAST::codegen() {
	std::vector<std::unique_ptr<DeclExprAST>> Decls = this->getDecls();
	std::vector<std::unique_ptr<StatAST>> Stats = this->getStats();

	for (auto &decl : Decls) {
		if (decl->codegen() == nullptr)
			return LogErrorV("declaration error");
	}
	for (int i = 0; i < Stats.size()-1;i++) {
		if (Value *RetVal = Stats.at(i)->codegen()) {
			if (Stats.at(i)->isRet()) {
				r = true;
			}
		}
		else {
			return LogErrorV("statement error");
		}
	}
	return Stats.at(Stats.size() - 1)->codegen();
}

///Moudle初始化
static void InitializeModuleAndPassManager() {
	// Open a new module.
	TheModule = llvm::make_unique<Module>("my cool jit", TheContext);
	TheModule->setDataLayout(TheJIT->getTargetMachine().createDataLayout());
	// Create a new pass manager attached to it.
	// Create a new pass manager attached to it.
	TheFPM = llvm::make_unique<legacy::FunctionPassManager>(TheModule.get());

	// Promote allocas to registers.
	/*TheFPM->add(createPromoteMemoryToRegisterPass());*/
	// Do simple "peephole" optimizations and bit-twiddling optzns.
	TheFPM->add(createInstructionCombiningPass());
	// Reassociate expressions.
	TheFPM->add(createReassociatePass());
	// Eliminate Common SubExpressions.
	TheFPM->add(createGVNPass());
	// Simplify the control flow graph (deleting unreachable blocks, etc).
	TheFPM->add(createCFGSimplificationPass());

	TheFPM->doInitialization();
}

///print相关↓
static std::vector<llvm::Value *> paramArrayRef;	//用于存储打印项
static Function *printFunc;							//打印函数
static std::vector<std::unique_ptr<ExprAST>> explist;			//用于存储表达式类型的打印项
static std::string strlist;							//用于存储TEXT类型的打印项

//声明printf函数
static void DeclarePrintfFunc()
{
	std::vector<llvm::Type *> printf_arg_types;
	printf_arg_types.push_back(Builder.getInt8Ty()->getPointerTo());//PointerType::get(Type::getInt8Ty(TheContext), 0)
	FunctionType *printType = FunctionType::get(
		IntegerType::getInt32Ty(TheContext), printf_arg_types, true);
	printFunc = llvm::Function::Create(printType, llvm::Function::ExternalLinkage,
		llvm::Twine("printf"), TheModule.get());
	printFunc->setCallingConv(llvm::CallingConv::C);

	std::vector<std::string> ArgNames;
	FunctionProtos["printf"] = std::move(llvm::make_unique<PrototypeAST>("printf", std::move(ArgNames)));

}

<<<<<<< HEAD
static void HandleFUNC() {
	if (auto ProtoAST = ParseFUNC()) {
		fprintf(stderr, "Parsed an FUNC\n");
		FnIR->print(errs());
		fprintf(stderr, "\n");
		FunctionProtos[ProtoAST->getName()] = std::move(ProtoAST);
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
=======
Value *TextPrintItemAST::codegen()
{
	strlist += Text.c_str();
	
	return Builder.getInt32(0);
>>>>>>> 16b48486c59db9f7ab4287b3b29c36cabb7752ab
}

Value *ExpPrintItemAST::codegen()
{
	std::string a = "%d";
	strlist += a;
	explist.push_back(std::move(Exp));
	return Builder.getInt32(0);
}

Value *PrintStatAST::codegen() {
	DeclarePrintfFunc();
	paramArrayRef.clear();
	explist.clear();
	strlist = "";
	Function *TheFunction = Builder.GetInsertBlock()->getParent();
	if (Items.size() == 0)
		return nullptr;
	else {
		
		for (int i = 0; i < Items.size(); i++) {
			Items[i]->codegen();
		}
	}
	paramArrayRef.push_back(Builder.CreateGlobalStringPtr(strlist.c_str()));
	for (int i = 0; i < explist.size(); i++) {
		paramArrayRef.push_back(explist.at(i)->codegen());
	}

	Builder.CreateCall(printFunc, paramArrayRef,"printcall");
	Value *num = Builder.getInt32(0);//print always return 0
	return num;
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
      //TheJIT->addModule(std::move(TheModule));
      //InitializeModuleAndPassManager();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

/// 处理外部表达式（用于测试使用，实际代码应删除）
static void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read top-level expression:");
      // FnIR->print(errs());
      fprintf(stderr, "\n");

      // JIT the module containing the anonymous expression, keeping a handle so
      // we can free it later.
      auto H = TheJIT->addModule(std::move(TheModule));
      InitializeModuleAndPassManager();

      // Search the JIT for the __anon_expr symbol.
      auto ExprSymbol = TheJIT->findSymbol("__anon_expr");
      assert(ExprSymbol && "Function not found");

      // Get the symbol's address and cast it to the right type (takes no
      // arguments, returns a double) so we can call it as a native function.
      int (*i)() = (int (*)())(intptr_t)cantFail(ExprSymbol.getAddress());
      fprintf(stderr, "Evaluated to %d\n", i());

      // Delete the anonymous expression module from the JIT.
      TheJIT->removeModule(H);
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}
static void HandleVariable() {
  if (ParseVariableExpr()) {
    fprintf(stderr, "Parsed a variable\n");
  } else {
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
    case VAR:
      HandleVariable();
      break;
    default:
      HandleTopLevelExpression();
      break;
    }
  }
}

//===----------------------------------------------------------------------===//
// "Library" functions that can be "extern'd" from user code.
//===----------------------------------------------------------------------===//

#ifdef LLVM_ON_WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT
#endif

/// putchard - putchar that takes a int and returns 0.
extern "C" DLLEXPORT int putchard(int X) {
	fputc((char)X, stderr);
	return 0;
}

/// printd - printf that takes a int prints it as "%i\n", returning 0.
extern "C" DLLEXPORT int printd(int X) {
	fprintf(stderr, "%i\n", X);
	return 0;
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//
int main() {

	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();

	// Install standard binary operators.
	// 1 is lowest precedence.
	BinopPrecedence[':='] = 2;
	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40; // highest.

							   // Prime the first token.
	fprintf(stderr, "ready> ");
	getNextToken();

	TheJIT = llvm::make_unique<KaleidoscopeJIT>();
	InitializeModuleAndPassManager();

	DeclarePrintfFunc();
	// Make the module, which holds all the code.
	// TheModule = llvm::make_unique<Module>("my cool jit", TheContext);

	// Run the main "interpreter loop" now.
	MainLoop();

	FILE *fp;
	//输出词法分析结果
	fp = fopen("lexer.txt", "w");
	for (int i = 0; i < cifa.size(); i++) {
		fprintf(fp, "%s\n", cifa[i].c_str());
	}
	fclose(fp);
	outs() << "Wrote "
		<< "lexer.txt"
		<< "\n";

	//输出语法分析结果
	fp = fopen("parser.txt", "w");
	for (int i = 0; i < yufa.size(); i++) {
		fprintf(fp, "%s", yufa[i].c_str());
	}
	fclose(fp);
	outs() << "Wrote "
		<< "parser.txt"
		<< "\n";
	// Print out all of the generated code.
	// TheModule->print(errs(), nullptr);

	// Initialize the target registry etc.
	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();

	auto TargetTriple = sys::getDefaultTargetTriple();
	TheModule->setTargetTriple(TargetTriple);

	std::string Error;
	auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

	// Print an error and exit if we couldn't find the requested target.
	// This generally occurs if we've forgotten to initialise the
	// TargetRegistry or we have a bogus target triple.
	if (!Target) {
		errs() << Error;
		return 1;
	}

	auto CPU = "generic";
	auto Features = "";

	TargetOptions opt;
	auto RM = Optional<Reloc::Model>();
	auto TheTargetMachine =
		Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

	TheModule->setDataLayout(TheTargetMachine->createDataLayout());

	auto Filename = "output.o";
	std::error_code EC;
	raw_fd_ostream dest(Filename, EC, sys::fs::F_None);

	if (EC) {
		errs() << "Could not open file: " << EC.message();
		return 1;
	}

	legacy::PassManager pass;
	auto FileType = TargetMachine::CGFT_ObjectFile;

	if (TheTargetMachine->addPassesToEmitFile(pass, dest, FileType)) {
		errs() << "TheTargetMachine can't emit a file of this type";
		return 1;
	}

	pass.run(*TheModule);
	dest.flush();

	outs() << "Wrote " << Filename << "\n";

	return 0;
}



