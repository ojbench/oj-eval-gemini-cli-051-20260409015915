#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <cctype>
#include <stdexcept>
#include <fstream>

using namespace std;

enum TokenType {
    ID, NUMBER, QUESTION, COLON, DOT, SEMICOLON, PLUS, MINUS, LPAREN, RPAREN,
    MSUBLEQ, RSUBLEQ, LDORST, END_OF_FILE
};

struct Token {
    TokenType type;
    string value;
    int line;
};

vector<Token> tokens;
int current_token = 0;

void tokenize(const string& input) {
    int i = 0;
    int line = 1;
    while (i < input.length()) {
        if (isspace(input[i])) {
            if (input[i] == '\n') line++;
            i++;
            continue;
        }
        if (input[i] == '/' && i + 1 < input.length() && input[i+1] == '/') {
            while (i < input.length() && input[i] != '\n') i++;
            continue;
        }
        if (input[i] == '/' && i + 1 < input.length() && input[i+1] == '*') {
            i += 2;
            while (i + 1 < input.length() && !(input[i] == '*' && input[i+1] == '/')) {
                if (input[i] == '\n') line++;
                i++;
            }
            i += 2;
            continue;
        }
        if (isalpha(input[i]) || input[i] == '_') {
            string id = "";
            while (i < input.length() && (isalnum(input[i]) || input[i] == '_')) {
                id += input[i];
                i++;
            }
            if (id == "msubleq") tokens.push_back({MSUBLEQ, id, line});
            else if (id == "rsubleq") tokens.push_back({RSUBLEQ, id, line});
            else if (id == "ldorst") tokens.push_back({LDORST, id, line});
            else tokens.push_back({ID, id, line});
            continue;
        }
        if (isdigit(input[i])) {
            string num = "";
            while (i < input.length() && isdigit(input[i])) {
                num += input[i];
                i++;
            }
            tokens.push_back({NUMBER, num, line});
            continue;
        }
        char c = input[i];
        if (c == '?') tokens.push_back({QUESTION, "?", line});
        else if (c == ':') tokens.push_back({COLON, ":", line});
        else if (c == '.') tokens.push_back({DOT, ".", line});
        else if (c == ';') tokens.push_back({SEMICOLON, ";", line});
        else if (c == '+') tokens.push_back({PLUS, "+", line});
        else if (c == '-') tokens.push_back({MINUS, "-", line});
        else if (c == '(') tokens.push_back({LPAREN, "(", line});
        else if (c == ')') tokens.push_back({RPAREN, ")", line});
        else {
            throw runtime_error("Unknown character: " + string(1, c));
        }
        i++;
    }
    tokens.push_back({END_OF_FILE, "", line});
}

map<string, int> label_map;

struct Expr {
    virtual ~Expr() {}
    virtual int eval(int current_item_address) = 0;
};

struct NumberExpr : Expr {
    int value;
    NumberExpr(int v) : value(v) {}
    int eval(int current_item_address) override { return value; }
};

struct QuestionExpr : Expr {
    int eval(int current_item_address) override { return current_item_address + 1; }
};

struct IdExpr : Expr {
    string name;
    IdExpr(string n) : name(n) {}
    int eval(int current_item_address) override {
        if (label_map.count(name) == 0) {
            throw runtime_error("Undefined label: " + name);
        }
        return label_map[name];
    }
};

struct BinaryExpr : Expr {
    char op;
    Expr* left;
    Expr* right;
    BinaryExpr(char o, Expr* l, Expr* r) : op(o), left(l), right(r) {}
    int eval(int current_item_address) override {
        if (op == '+') return left->eval(current_item_address) + right->eval(current_item_address);
        if (op == '-') return left->eval(current_item_address) - right->eval(current_item_address);
        return 0;
    }
};

struct UnaryExpr : Expr {
    char op;
    Expr* expr;
    UnaryExpr(char o, Expr* e) : op(o), expr(e) {}
    int eval(int current_item_address) override {
        if (op == '-') return -expr->eval(current_item_address);
        return 0;
    }
};

Expr* parse_expression();

Expr* parse_term() {
    if (tokens[current_token].type == MINUS) {
        current_token++;
        return new UnaryExpr('-', parse_term());
    } else if (tokens[current_token].type == LPAREN) {
        current_token++;
        Expr* e = parse_expression();
        if (tokens[current_token].type != RPAREN) throw runtime_error("Expected )");
        current_token++;
        return e;
    } else if (tokens[current_token].type == NUMBER) {
        int val = stoi(tokens[current_token].value);
        current_token++;
        return new NumberExpr(val);
    } else if (tokens[current_token].type == QUESTION) {
        current_token++;
        return new QuestionExpr();
    } else if (tokens[current_token].type == ID) {
        string name = tokens[current_token].value;
        current_token++;
        return new IdExpr(name);
    } else {
        throw runtime_error("Expected term");
    }
}

Expr* parse_expression() {
    Expr* left = parse_term();
    while (tokens[current_token].type == PLUS || tokens[current_token].type == MINUS) {
        char op = tokens[current_token].type == PLUS ? '+' : '-';
        current_token++;
        Expr* right = parse_term();
        left = new BinaryExpr(op, left, right);
    }
    return left;
}

struct Item {
    Expr* expr;
    int address;
    bool is_copy;
};

struct Instruction {
    int opcode; // -1 for '.', 0 for msubleq, 1 for rsubleq, 2 for ldorst
    int address;
    vector<Item> items;
};

vector<Instruction> instructions;
int current_address = 0;

void parse_program() {
    while (tokens[current_token].type != END_OF_FILE) {
        while (tokens[current_token].type == ID && tokens[current_token+1].type == COLON) {
            label_map[tokens[current_token].value] = current_address;
            current_token += 2;
        }
        
        if (tokens[current_token].type == END_OF_FILE) break;

        Instruction inst;
        inst.address = current_address;
        
        if (tokens[current_token].type == DOT) {
            inst.opcode = -1;
            current_token++;
        } else if (tokens[current_token].type == MSUBLEQ) {
            inst.opcode = 0;
            current_address++;
            current_token++;
        } else if (tokens[current_token].type == RSUBLEQ) {
            inst.opcode = 1;
            current_address++;
            current_token++;
        } else if (tokens[current_token].type == LDORST) {
            inst.opcode = 2;
            current_address++;
            current_token++;
        } else {
            throw runtime_error("Expected opcode");
        }

        while (tokens[current_token].type != SEMICOLON) {
            while (tokens[current_token].type == ID && tokens[current_token+1].type == COLON) {
                label_map[tokens[current_token].value] = current_address;
                current_token += 2;
            }
            if (tokens[current_token].type == SEMICOLON) break;
            
            Item item;
            item.expr = parse_expression();
            item.address = current_address;
            item.is_copy = false;
            inst.items.push_back(item);
            current_address++;
        }
        current_token++; // skip ';'

        if (inst.opcode == 0 || inst.opcode == 1) {
            if (inst.items.size() == 1) {
                Item copy_item;
                copy_item.expr = nullptr;
                copy_item.address = current_address;
                copy_item.is_copy = true;
                inst.items.push_back(copy_item);
                current_address++;

                Item q_item;
                q_item.expr = new QuestionExpr();
                q_item.address = current_address;
                q_item.is_copy = false;
                inst.items.push_back(q_item);
                current_address++;
            } else if (inst.items.size() == 2) {
                Item q_item;
                q_item.expr = new QuestionExpr();
                q_item.address = current_address;
                q_item.is_copy = false;
                inst.items.push_back(q_item);
                current_address++;
            }
        }
        instructions.push_back(inst);
    }
}

int main() {
    string input((istreambuf_iterator<char>(cin)), istreambuf_iterator<char>());
    try {
        tokenize(input);
        parse_program();
        
        vector<int> memory(current_address, 0);
        for (auto& inst : instructions) {
            if (inst.opcode != -1) {
                memory[inst.address] = inst.opcode;
            }
            int first_val = 0;
            for (int i = 0; i < inst.items.size(); i++) {
                auto& item = inst.items[i];
                if (item.is_copy) {
                    memory[item.address] = first_val;
                } else {
                    int val = item.expr->eval(item.address);
                    memory[item.address] = val;
                    if (i == 0) first_val = val;
                }
            }
        }
        
        for (auto& inst : instructions) {
            if (inst.opcode != -1) {
                cout << memory[inst.address] << " ";
            }
            for (int i = 0; i < inst.items.size(); i++) {
                cout << memory[inst.items[i].address] << " ";
            }
            cout << endl;
        }
    } catch (const exception& e) {
        cerr << "Error: " << e.what() << endl;
        return 1;
    }
    return 0;
}
