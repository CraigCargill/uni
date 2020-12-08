using Compiler.IO;
using Compiler.Tokenization;
using System.Collections.Generic;
using static Compiler.Tokenization.TokenType;
using Compiler.Nodes;

namespace Compiler.SyntacticAnalysis
{
    /// <summary>
    /// A recursive descent parser
    /// </summary>
    public class Parser
    {
        /// <summary>
        /// The error reporter
        /// </summary>
        public ErrorReporter Reporter { get; }

        /// <summary>
        /// The tokens to be parsed
        /// </summary>
        private List<Token> tokens;

        /// <summary>
        /// The index of the current token in tokens
        /// </summary>
        private int currentIndex;

        /// <summary>
        /// The current token
        /// </summary>
        private Token CurrentToken { get { return tokens[currentIndex]; } }

        /// <summary>
        /// Advances the current token to the next one to be parsed
        /// </summary>
        private void MoveNext()
        {
            if (currentIndex < tokens.Count - 1)
                currentIndex += 1;
        }

        /// <summary>
        /// Creates a new parser
        /// </summary>
        /// <param name="reporter">The error reporter to use</param>
        public Parser(ErrorReporter reporter)
        {
            Reporter = reporter;
        }

        /// <summary>
        /// Checks the current token is the expected kind and moves to the next token
        /// </summary>
        /// <param name="expectedType">The expected token type</param>
        private void Accept(TokenType expectedType)
        {
            if (CurrentToken.Type == expectedType)
            {
                Debugger.Write($"Accepted {CurrentToken}");
                MoveNext();
            }
        }

        public ProgramNode Parse(List<Token> tokens)
        {
            this.tokens = tokens;
            ProgramNode program = ParseProgram();
            return program;
        }

        // Program -------------------------------------------------------------

        // Parse Program -----------------------------

        /// <summary>
        /// Parses a program
        /// </summary>
        /// <returns> An abstract syntax tree representing the program </returns>
        private ProgramNode ParseProgram()
        {
            Debugger.Write("Parsing Program");
            ICommandNode command = ParseCommand();
            return new ProgramNode(command);
        }

        // Commands ------------------------------------------------------------

        // Parse Command -----------------------------

        /// <summary>
        /// Parses a command
        /// </summary>
        /// <returns> An abstract syntax tree representing the command </returns>
        private ICommandNode ParseCommand()
        {
            Debugger.Write("Parsing Command");
            List<ICommandNode> commands = new List<ICommandNode>();
            commands.Add(ParseSingleCommand());
            while (CurrentToken.Type == Semicolon)
            {
                Accept(Semicolon);
                commands.Add(ParseSingleCommand());
            }
            if (commands.Count == 1)
            {
                return commands[0];
            }
            else
            {
                return new SequentialCommandNode(commands);
            }
        }

        // Parse Single Command ----------------------

        /// <summary>
        /// Parses a single command
        /// </summary>
        /// <returns> An abstract syntax tree representing the single command </returns>
        private ICommandNode ParseSingleCommand()
        {
            Debugger.Write("Parsing Single Command");
            switch (CurrentToken.Type)
            {
                case Identifier:
                    return ParseAssignmentOrCallCommand();
                case If:
                    return ParseIfCommand();
                case While:
                    return ParseWhileCommand();
                case Let:
                    return ParseLetCommand();
                case Begin:
                    return ParseBeginCommand();
                case For:
                    return ParseForCommand();
                default:
                    return new ErrorNode(CurrentToken.Position);
            }
        }

        // Parse Assignment or Call Command ----------

        /// <summary>
        /// Parses an assignment or call command
        /// </summary>
        /// <returns> An abstract syntax tree representing the assign and call command </returns>
        private ICommandNode ParseAssignmentOrCallCommand()
        {
            Debugger.Write("Parsing Assignment Command or Call Command");
            Position startPosition = CurrentToken.Position;
            IdentifierNode identifier = ParseIdentifier();

            if (CurrentToken.Type == Becomes)
            {
                Debugger.Write("Parsing Assignment Command");
                Accept(Becomes);
                IExpressionNode expression = ParseExpression();
                return new AssignCommandNode(identifier, expression);
            }
            else if (CurrentToken.Type == LeftBracket)
            {
                Debugger.Write("Parsing Call Command");
                Accept(LeftBracket);
                IParameterNode parameter = ParseParameter();
                Accept(RightBracket);
                return new CallCommandNode(identifier, parameter);
            }
            else
            {
                return new ErrorNode(startPosition);
            }
        }

        // Parse If Command --------------------------

        /// <summary>
        /// Parses an if command
        /// </summary>
        /// <returns> An abstract syntax tree representing the if command </returns>
        private ICommandNode ParseIfCommand()
        {
            Debugger.Write("Parsing If Command");
            Position startPosition = CurrentToken.Position;
            Accept(If);
            Accept(LeftBracket);
            IExpressionNode expression = ParseExpression();
            Accept(RightBracket);
            Accept(Then);
            ICommandNode thenCommand = ParseSingleCommand();
            
            switch (CurrentToken.Type)
            {
                case (Else):
                    Accept(Else);
                    ICommandNode elseCommand = ParseSingleCommand();
                    Accept(EndIf);
                    return new IfElseCommandNode(expression, thenCommand, elseCommand, startPosition);
            }
            Accept(EndIf);
            return new IfCommandNode(expression, thenCommand, startPosition);
        }

        // Parse While Command -----------------------

        /// <summary>
        /// Parses a while command
        /// </summary>
        /// <returns> An abstract syntax tree representing the while command </returns>
        private ICommandNode ParseWhileCommand()
        {
            Debugger.Write("Parsing While Command");
            Position startPosition = CurrentToken.Position;
            Accept(While);
            Accept(LeftBracket);
            IExpressionNode expression = ParseExpression();
            Accept(RightBracket);
            Accept(Do);
            ICommandNode command = ParseSingleCommand();
            return new WhileCommandNode(expression, command, startPosition);
        }

        // Parse Let Command -------------------------

        /// <summary>
        /// Parses a let command
        /// </summary>
        /// <returns> An abstract syntax tree representing the let command </returns>
        private ICommandNode ParseLetCommand()
        {
            Debugger.Write("Parsing Let Command");
            Position startPosition = CurrentToken.Position;
            Accept(Let);
            IDeclarationNode declaration = ParseDeclaration();
            Accept(In);
            ICommandNode command = ParseSingleCommand();
            return new LetCommandNode(declaration, command, startPosition);
        }

        // Parse Begin Command -----------------------

        /// <summary>
        /// Parses a begin command
        /// </summary>
        /// <returns> An abstract syntax tree representing the begin command </returns>
        private ICommandNode ParseBeginCommand()
        {
            Debugger.Write("Parsing Begin Command");
            Accept(Begin);
            ICommandNode command = ParseCommand();
            Accept(End);
            return command;
        }

        // Parse For Command -------------------------

        /// <summary>
        /// Parses a for command
        /// </summary>
        /// <returns> An abstract syntax tree representing the for command </returns>
        private ICommandNode ParseForCommand()
        {
            Debugger.Write("Parsing For Command");
            Position startPosition = CurrentToken.Position;
            Accept(For);
            IdentifierNode identifier = ParseIdentifier();
            Accept(Becomes);
            IExpressionNode becomesExpression = ParseExpression();
            Accept(To);
            IExpressionNode toExpression = ParseExpression();
            Accept(Do);
            ICommandNode command = ParseSingleCommand();
            Accept(Next);
            return new ForCommandNode(identifier, becomesExpression, toExpression, command, startPosition);
        }

        // Declarations --------------------------------------------------------

        // Parse Declaration -------------------------

        /// <summary>
        /// Parses a declaration
        /// </summary>
        /// <returns> An abstract syntax tree representing the declaration </returns>
        private IDeclarationNode ParseDeclaration()
        {
            Debugger.Write("Parsing Declaration");
            List<IDeclarationNode> declarations = new List<IDeclarationNode>();
            declarations.Add(ParseSingleDeclaration());
            while (CurrentToken.Type == Semicolon)
            {
                Accept(Semicolon);
                declarations.Add(ParseSingleDeclaration());
            }
            if (declarations.Count == 1)
            {
                return declarations[0];
            }
            else
            {
                return new DeclarationNode(declarations);
            }
        }

        // Parse Single Declaration ------------------

        /// <summary>
        /// Parses a single declaration
        /// </summary>
        /// <returns> An abstract syntax tree representing the single declaration </returns>
        private IDeclarationNode ParseSingleDeclaration()
        {
            Debugger.Write("Parsing Single Declaration");
            switch (CurrentToken.Type)
            {
                case Const:
                    return ParseConstDeclaration();
                case Var:
                    return ParseVarDeclaration();
                default:
                    return new ErrorNode(CurrentToken.Position);
            }
        }

        // Parse Const Declaration -------------------

        /// <summary>
        /// Parses a constant declaration
        /// </summary>
        /// <returns> An abstract syntax tree representing the const declaration </returns>
        private IDeclarationNode ParseConstDeclaration()
        {
            Debugger.Write("Parsing Constant Declaration");
            Position startPosition = CurrentToken.Position;
            Accept(Const);
            IdentifierNode identifier = ParseIdentifier();
            Accept(Is);
            IExpressionNode expression = ParseExpression();
            return new ConstDeclarationNode(identifier, expression, startPosition);
        }

        // Parse Var Declaration ---------------------

        /// <summary>
        /// Parses a variable declaration
        /// </summary>
        /// <returns> An abstract syntax tree representing the var declaration </returns>
        private IDeclarationNode ParseVarDeclaration()
        {
            Debugger.Write("Parsing Variable Declaration");
            Position startPosition = CurrentToken.Position;
            Accept(Var);
            IdentifierNode identifier = ParseIdentifier();
            Accept(Colon);
            TypeNameNode typeDenoter = ParseTypeDenoter();
            return new VarDeclarationNode(identifier, typeDenoter, startPosition);
        }

        // Parse Parameter -----------------------------------------------------

        /// <summary>
        /// Parses a parameter
        /// </summary>
        /// <returns> An abstract syntax tree representing the parameter </returns>
        private IParameterNode ParseParameter()
        {
            Debugger.Write("Parsing Parameter");
            switch (CurrentToken.Type)
            {
                case Identifier:
                case IntLiteral:
                case CharLiteral:
                case Operator:
                case LeftBracket:
                    return ParseValueParameter();
                case RightBracket:
                    return ParseEmptyParameter();
                case Var:
                    return ParseVarParameter();
                default:
                    return new ErrorNode(CurrentToken.Position);
            }
        }

        // Parse Empty Parameter ---------------------

        /// <summary>
        /// Parses a empty parameter
        /// </summary>
        /// <returns>An abstract syntax tree representing the empty parameter</returns>
        private IParameterNode ParseEmptyParameter()
        {
            Debugger.Write("Parsing Empty Parameter");
            Position startPosition = CurrentToken.Position;
            return new EmptyParameterNode(startPosition);
        }

        // Parse Var Parameter -----------------------

        /// <summary>
        /// Parses a variable parameter
        /// </summary>
        /// <returns> An abstract syntax tree representing the var parameter </returns>
        private IParameterNode ParseVarParameter()
        {
            Debugger.Write("Parsing Variable Parameter");
            Position startPosition = CurrentToken.Position;
            Accept(Var);
            IdentifierNode identifier = ParseIdentifier();
            return new VarParameterNode(identifier, startPosition);
        }

        // Parse Value Parameter ---------------------

        /// <summary>
        /// Parses a value parameter
        /// </summary>
        /// <returns> An abstract syntax tree representing the val declaration </returns>
        private IParameterNode ParseValueParameter()
        {
            Debugger.Write("Parsing Value Parameter");
            IExpressionNode expression = ParseExpression();
            return new ValueParameterNode(expression);
        }

        // Parse Type Denoter --------------------------------------------------

        /// <summary>
        /// Parses a type denoter
        /// </summary>
        /// <returns> An abstract syntax tree representing the type denoter </returns>
        private TypeNameNode ParseTypeDenoter()
        {
            Debugger.Write("Parsing Type Denoter");
            IdentifierNode identifier = ParseIdentifier();
            return new TypeNameNode(identifier);
        }

        // Expression ----------------------------------------------------------

        // Parse Expression --------------------------

        /// <summary>
        /// Parses an expression
        /// </summary>
        /// <returns> An abstract syntax tree representing the expression </returns>
        private IExpressionNode ParseExpression()
        {
            Debugger.Write("Parsing Expression");
            IExpressionNode firstExpression = ParsePrimaryExpression();
            while (CurrentToken.Type == Operator)
            {
                OperatorNode operation = ParseOperator();
                IExpressionNode secondExpression = ParsePrimaryExpression();
                firstExpression = new BinaryExpressionNode(firstExpression, operation, secondExpression);
            }
            return firstExpression;
        }

        // Parse Primary Expression ------------------

        /// <summary>
        /// Parses a primary expression
        /// </summary>
        private IExpressionNode ParsePrimaryExpression()
        {
            Debugger.Write("Parsing Primary Expression");
            switch (CurrentToken.Type)
            {
                case IntLiteral:
                    return ParseIntExpression();
                case CharLiteral:
                    return ParseCharExpression();
                case Identifier:
                    return ParseIdExpression();
                //case Call:
                //    return ParseCallExpression(); Call within id expression
                //case Binary:
                //    return ParseBinaryExpression();
                case Operator:
                    return ParseUnaryExpression();
                case LeftBracket:
                    return ParseBracketExpression();
                default:
                    IExpressionNode leftExpression = ParseExpression();
                    if (CurrentToken.Type == Operator)
                    {
                        OperatorNode op = ParseOperator();
                        IExpressionNode rightExpression = ParseExpression();
                        return new BinaryExpressionNode(leftExpression, op, rightExpression);
                    }
                    else
                    {
                        return new ErrorNode(CurrentToken.Position);
                    }
            }
        }

        // Parse Int Expression ----------------------

        /// <summary>
        /// Parses an int expression
        /// </summary>
        /// <returns> An abstract syntax tree representing the int expression </returns>
        private IExpressionNode ParseIntExpression()
        {
            Debugger.Write("Parsing Int Expression");
            IntegerLiteralNode intLit = ParseIntegerLiteral();
            return new IntegerExpressionNode(intLit);
        }

        /// <summary>
        /// Parses an integer literal
        /// </summary>
        /// <returns> An abstract syntax tree representing the integer literal </returns>
        private IntegerLiteralNode ParseIntegerLiteral()
        {
            Debugger.Write("Parsing Integer Literal");
            Token integerLiteralToken = CurrentToken;
            Accept(IntLiteral);
            return new IntegerLiteralNode(integerLiteralToken);
        }

        // Parse Char Expression ---------------------

        /// <summary>
        /// Parses a char expression
        /// </summary>
        /// <returns> An abstract syntax tree representing the char expression </returns>
        private IExpressionNode ParseCharExpression()
        {
            Debugger.Write("Parsing Char Expression");
            CharacterLiteralNode charLit = ParseCharacterLiteral();
            return new CharExpressionNode(charLit);
        }

        /// <summary>
        /// Parses a character literal
        /// </summary>
        /// <returns> An abstract syntax tree representing the character literal </returns>
        private CharacterLiteralNode ParseCharacterLiteral()
        {
            Debugger.Write("Parsing Character Literal");
            Token charLiteralToken = CurrentToken;
            Accept(CharLiteral);
            return new CharacterLiteralNode(charLiteralToken);
        }

        // Parse ID Expression -----------------------

        /// <summary>
        /// Parses an ID expression
        /// </summary>
        /// <returns> An abstract syntax tree representing the id expression </returns>
        private IExpressionNode ParseIdExpression()
        {
            Debugger.Write("Parsing Identifier Expression");
            IdentifierNode identifier = ParseIdentifier();

            if (CurrentToken.Type == LeftBracket)
            {
                Accept(LeftBracket);
                IParameterNode parameter = ParseParameter();
                Accept(RightBracket);
                return new CallExpressionNode(identifier, parameter);
            }
            else
            {
                return new IdExpressionNode(identifier);
            }
        }

        // Parse Call Expression ---------------------

        ///// <summary>
        ///// Parses an call expression
        ///// </summary>
        ///// <returns> An abstract syntax tree representing the call expression </returns>
        //private IExpressionNode ParseCallExpression()
        //{
        //    Debugger.Write("Parsing Call Expression");
        //    IdentifierNode identifier = ParseIdentifier();
        //    Accept(LeftBracket);
        //    IParameterNode parameter = ParseParameter();
        //    Accept(RightBracket);
        //    return new CallExpressionNode(identifier, parameter);
        //}

        // Parse Binary Expression -------------------

        ///// <summary>
        ///// Parses an binary expression
        ///// </summary>
        ///// <returns> An abstract syntax tree representing the binary expression </returns>
        //private IExpressionNode ParseBinaryExpression()
        //{
        //    Debugger.Write("Parsing Binary Expression");
        //    IExpressionNode leftExpression = ParseExpression();
        //    OperatorNode operation = ParseOperator();
        //    IExpressionNode rightExpression = ParseExpression();
        //    return new BinaryExpressionNode(leftExpression, operation, rightExpression);
        //}

        // Parse Unary Expression --------------------

        /// <summary>
        /// Parses a unary expression
        /// </summary>
        /// <returns> An abstract syntax tree representing the unary expression </returns>
        private IExpressionNode ParseUnaryExpression()
        {
            Debugger.Write("Parsing Unary Expression");
            OperatorNode operation = ParseOperator();
            IExpressionNode expression = ParsePrimaryExpression();
            return new UnaryExpressionNode(operation, expression);
        }

        // Parse Bracket Expression ------------------

        /// <summary>
        /// Parses a bracket expression
        /// </summary>
        /// <returns> An abstract syntax tree representing the bracket expression </returns>
        private IExpressionNode ParseBracketExpression()
        {
            Debugger.Write("Parsing Bracket Expression");
            Accept(LeftBracket);
            IExpressionNode expression = ParseExpression();
            Accept(RightBracket);
            return expression;
        }

        // Parse Additional Methods --------------------------------------------


        // Parse Identifier --------------------------

        /// <summary>
        /// Parses an identifier
        /// </summary>
        /// <returns>An abstract syntax tree representing the identifier</returns>
        private IdentifierNode ParseIdentifier()
        {
            Debugger.Write("Parsing Identifier");
            Token identifierToken = CurrentToken;
            Accept(Identifier);
            return new IdentifierNode(identifierToken);
        }

        // Parse Operator ----------------------------

        /// <summary>
        /// Parses an operator
        /// </summary>
        /// <returns>An abstract syntax tree representing the operator</returns>
        private OperatorNode ParseOperator()
        {
            Debugger.Write("Parsing Operator");
            Token operatorToken = CurrentToken;
            Accept(Operator);
            return new OperatorNode(operatorToken);
        }

    }
}