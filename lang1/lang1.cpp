/*
		Interpreter of small programming language.
		I has following syntax: {} - 0 or more, [] - 0 or 1, () - grouping

		program            := {statement}
		statement          := assign | condition | compound_statement
		compound_statement := "{" {statement} "}"
		assign             := id "=" expr ";"
		condition          := "if" "(" expr ")" statement ["else" statement]
		expr               := operand {op operand}
		operand            := ["-"] (id | number)
		op                 := "+" | "-" | "*" | "/" | "==" | "!=" | ">" | "<" | "<=" | ">="
		number             := [0-9]+
		id                 := [a-zA-Z][a-zA-Z_0-9]*
*/
#include <cassert>
#include <cctype>
#include <iostream>
#include <map>
#include <string>
#include <vector>
#include <variant>
#include "magic_enum.hpp"

enum class TokenType {
	IF,
	ELSE,
	ID,
	NUMBER,
	SEMICOLON,
	LPAREN,
	RPAREN,
	LCURLY,
	RCURLY,
	PLUS,
	MINUS,
	STAR,
	SLASH,
	EQUAL,      // ==
	NOT_EQUAL,  // !=
	ASSIGN,     // =
	GREATER,
	GREATER_EQUAL,
	LESS,
	LESS_EQUAL,
	ENDOFFILE
};

enum class AstNodeType {
	PROGRAM,
	COMPOUND_ST,
	CONDITION,
	ASSIGN,
	VAR,
	NUMBER,

	ADD,
	SUBTRACT,
	MULTIPLY,
	DIVIDE,
	EQUAL,
	NOT_EQUAL,
	GREATER,
	GREATER_EQUAL,
	LESS,
	LESS_EQUAL,

	INVALID
};

struct Token {
	TokenType type;
	std::variant<int, std::string> value;

	Token() { }
	Token(TokenType type, int val) : type(type), value(val) {}
	Token(TokenType type, std::string& val) : type(type), value(val) {}
	Token(const Token& t) {
		type = t.type;
		value = t.value;
	}
	~Token() {}
};

struct AstNode {
	AstNodeType type;
	bool unary_minus; // with unary minus or not
	std::variant<int, std::string> value;
	std::vector<AstNode*> children;

	void push_child(AstNode* child) { children.push_back(child); }

	static void showAst(AstNode *root, int level = 0) {
		for (int i = 0; i < level; i++) {
			std::cout << "    ";
		}
		std::cout << magic_enum::enum_name(root->type) << "\n";
		for (AstNode *child : root->children) {
			showAst(child, level + 1);
		}
	}

	AstNode(AstNodeType type) : type(type), unary_minus(false) {}
	~AstNode() {};
};

std::string token_to_string(const Token& token) {
	std::string str;
	str += "{ type = ";
	if (token.type == TokenType::ID) {
		str += ("ID; value = " + std::get<std::string>(token.value) + "; }");
	}
	else if (token.type == TokenType::NUMBER) {
		str += (std::string("NUMBER; value = ") + std::to_string(std::get<int>(token.value)) + "; }");
	}
	else {
		str += magic_enum::enum_name(token.type);
		str += "; value = None; }";
	}
	return str;
}

std::vector<Token> tokenize(const std::string& text) {
	std::vector<Token> tokens;
	size_t pos = 0, text_len = text.length();
	while (pos < text_len) {
		char c = text[pos++];
		if (isalpha(c) || c == '_') {
			std::string word;
			while (pos < text_len && (isalpha(c) || c == '_' || isdigit(c))) {
				word += c;
				c = text[pos++];
			}
			pos--;
			if (word == "if") {
				tokens.push_back(Token(TokenType::IF, word));
			}
			else if (word == "else") {
				tokens.push_back(Token(TokenType::ELSE, word));
			}
			else {
				tokens.push_back(Token(TokenType::ID, word));
			}
		}
		else if (isdigit(c)) {
			int num = 0;
			while (pos < text_len && isdigit(c)) {
				num = num * 10 + (c - '0');
				c = text[pos++];
			}
			pos--;
			tokens.push_back(Token(TokenType::NUMBER, num));
		}
		else if (c == '=') {
			if (pos < text_len && text[pos] == '=') {
				pos++;
				tokens.push_back(Token(TokenType::EQUAL, 0));
			}
			else {
				tokens.push_back(Token(TokenType::ASSIGN, 0));
			}
		}
		else if (c == '!') {
			if (pos < text_len && text[pos] == '=') {
				pos++;
				tokens.push_back(Token(TokenType::NOT_EQUAL, 0));
			}
			else {
				throw std::string("Unexpected token: !");
			}
		}
		else if (c == '<') {
			if (pos < text_len && text[pos] == '=') {
				pos++;
				tokens.push_back(Token(TokenType::LESS_EQUAL, 0));
			}
			else {
				tokens.push_back(Token(TokenType::LESS, 0));
			}
		}
		else if (c == '>') {
			if (pos < text_len && text[pos] == '=') {
				pos++;
				tokens.push_back(Token(TokenType::GREATER_EQUAL, 0));
			}
			else {
				tokens.push_back(Token(TokenType::GREATER, 0));
			}
		}
		else if (c == ';') {
			tokens.push_back(Token(TokenType::SEMICOLON, 0));
		}
		else if (c == '(') {
			tokens.push_back(Token(TokenType::LPAREN, 0));
		}
		else if (c == ')') {
			tokens.push_back(Token(TokenType::RPAREN, 0));
		}
		else if (c == '{') {
			tokens.push_back(Token(TokenType::LCURLY, 0));
		}
		else if (c == '}') {
			tokens.push_back(Token(TokenType::RCURLY, 0));
		}
		else if (c == '+') {
			tokens.push_back(Token(TokenType::PLUS, 0));
		}
		else if (c == '-') {
			tokens.push_back(Token(TokenType::MINUS, 0));
		}
		else if (c == '*') {
			tokens.push_back(Token(TokenType::STAR, 0));
		}
		else if (c == '/') {
			tokens.push_back(Token(TokenType::SLASH, 0));
		}
		else if (c != '\n' && c != '\r' && c != ' ') {
			throw std::string("Unexpected token: ") + c;
		}
	}
	tokens.push_back(Token(TokenType::ENDOFFILE, 0));
	return tokens;
}

// Convert binary operation token type to corresponding ast node type
// return AstNodeType::INVALID if token type is not a binary operation
AstNodeType token_binop_to_ast_node_type(const Token &t) {
	AstNodeType ast_node_type = AstNodeType::INVALID;
	if (t.type == TokenType::PLUS) ast_node_type = AstNodeType::ADD;
	else if (t.type == TokenType::MINUS) ast_node_type = AstNodeType::SUBTRACT;
	else if (t.type == TokenType::STAR) ast_node_type = AstNodeType::MULTIPLY;
	else if (t.type == TokenType::SLASH) ast_node_type = AstNodeType::DIVIDE;
	else if (t.type == TokenType::EQUAL) ast_node_type = AstNodeType::EQUAL;
	else if (t.type == TokenType::NOT_EQUAL) ast_node_type = AstNodeType::NOT_EQUAL;
	else if (t.type == TokenType::GREATER) ast_node_type = AstNodeType::GREATER;
	else if (t.type == TokenType::GREATER_EQUAL) ast_node_type = AstNodeType::GREATER_EQUAL;
	else if (t.type == TokenType::LESS) ast_node_type = AstNodeType::LESS;
	else if (t.type == TokenType::LESS_EQUAL) ast_node_type = AstNodeType::LESS_EQUAL;
	return ast_node_type;
}

class Parser {
private:
	std::vector<Token> tokens;
	size_t pos;

public:
	Parser(std::vector<Token>&& t) : tokens(std::move(t)), pos(0) {}
	~Parser() {}

	AstNode* parse_operand() {
		std::cout << "parse_operand()\n";
		bool unary_minus = false;
		if (tokens[pos].type == TokenType::MINUS) {
			unary_minus = true;
			pos++;
		}
		if (tokens[pos].type == TokenType::ID) {
			AstNode* operand = new AstNode(AstNodeType::VAR);
			operand->unary_minus = unary_minus;
			operand->value = std::get<std::string>(tokens[pos].value);
			pos++;
			return operand;
		}
		else if (tokens[pos].type == TokenType::NUMBER) {
			AstNode* operand = new AstNode(AstNodeType::NUMBER);
			operand->unary_minus = unary_minus;
			operand->value = std::get<int>(tokens[pos].value);
			pos++;
			return operand;
		}
		else {
			return nullptr;
			// throw std::string("Syntax error: expected ID or NUMBER");
		}
	}

	AstNode* parse_expr() {
		std::cout << "parse_expr()"/* << magic_enum::enum_name(tokens[pos].type)*/ <<  "\n";
		AstNode* operand1 = parse_operand();
		if (operand1 == nullptr) {
			throw std::string("Syntax error: expected expression\n");
		}
		AstNodeType binop_type;
		if ((binop_type = token_binop_to_ast_node_type(tokens[pos])) != AstNodeType::INVALID) {
			AstNode* operand;
			while ((binop_type = token_binop_to_ast_node_type(tokens[pos])) != AstNodeType::INVALID) {
				pos++;
				operand = parse_operand();
				if (operand == nullptr) {
					throw std::string("Syntax error: expected operand\n");
				}
				AstNode* binop = new AstNode(binop_type);
				binop->push_child(operand1);
				binop->push_child(operand);
				operand1 = binop;
			}
		}
		return operand1;
	}

	AstNode* parse_statement() {
		std::cout << "parse_statement()\n";
		if (tokens[pos].type == TokenType::ID && tokens[pos + 1].type == TokenType::ASSIGN) {
			
			AstNode* assign = new AstNode(AstNodeType::ASSIGN);
			AstNode* lval = new AstNode(AstNodeType::VAR);
			lval->value = std::get<std::string>(tokens[pos].value);
			assign->push_child(lval);
			pos += 2;
			assign->push_child(parse_expr());
			if (tokens[pos].type != TokenType::SEMICOLON) {
				throw std::string("Syntax error: expected ;");
			}
			pos++;
			return assign;
		}
		else if (tokens[pos].type == TokenType::IF) {
			AstNode* cond = new AstNode(AstNodeType::CONDITION);
			if (tokens[pos + 1].type != TokenType::LPAREN) {
				throw std::string("Syntax error: expected (");
			}
			pos += 2;
			// std::cout << "cond->push_child(parse_expr())...\n";
			cond->push_child(parse_expr());
			// std::cout << "after cond->push_child(parse_expr())\n";
			if (tokens[pos].type != TokenType::RPAREN) {
				throw std::string("Syntax error: expected )");
			}
			pos++;
			cond->push_child(parse_statement());
			if (tokens[pos].type == TokenType::ELSE) {
				pos++;
				cond->push_child(parse_statement());
			}
			return cond;
		}
		else if (tokens[pos].type == TokenType::LCURLY) {
			pos++;
			AstNode* compound = new AstNode(AstNodeType::COMPOUND_ST);
			AstNode* stat;
			while ((stat = parse_statement()) != nullptr) {
				compound->push_child(stat);
			}
			if (tokens[pos].type != TokenType::RCURLY) {
				throw std::string("Syntax error: expected }");
			}
			return compound;
		}
		return nullptr;
	}

	AstNode* parse_program() {
		std::cout << "parse_program()\n";
		AstNode* program = new AstNode(AstNodeType::PROGRAM);
		AstNode* stat;
		while ((stat = parse_statement()) != nullptr) {
			program->push_child(stat);
		}
		return program;
	}
};

/*
		Receives program code, runs it
		Returns state of variables after running program
*/
std::map<std::string, int> eval(const std::string& program) {
	Parser parser(std::move(tokenize(program)));
	AstNode* root = parser.parse_program();
	return { {"aboba", 1337} };
}

/*
		Runs program and compares its result with given state
*/
bool run_test(const std::string& program,
			  const std::map<std::string, int>& needed_variable_state) {
	auto result_state = eval(program);
	if (result_state.size() != needed_variable_state.size()) {
		return false;
	}
	for (const auto& item : needed_variable_state) {
		if (result_state[item.first] != item.second) {
			return false;
		}
	}
	return true;
}

int main() {
	// assert(run_test(std::string("x = 1337;"), {{"x", 1337}}));
	// std::cout << "TESTS PASSED\n";

	auto tokens = tokenize(std::string(
		"myvar1=123+7*9;"
		"ans=0;"
		"if(myvar1==63+123)"
		"ans=1;"
		"if (1==1) {if (2>1) a = 7; else b = 9;}"
	));
	for (const Token& t : tokens) {
		std::cout << token_to_string(t) << "\n";
	}

	Parser parser(std::move(tokens));
	try {
		AstNode* root = parser.parse_program();
		// std::cout << magic_enum::enum_name(root->children[0]->children[0]->type);
		std::cout << "AST:\n";
		AstNode::showAst(root);
	}
	catch (const std::string &s) {
		std::cout << s;
	}
	
	
}