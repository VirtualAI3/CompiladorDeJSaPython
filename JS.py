import ply.lex as lex
import ply.yacc as yacc

symbol_table = {}
tabla_errores=[]
lineas = 0

# Definición de tokens
tokens = (
    'LET',
    'VAR',
    'CONST',
    'ID',
    'SEMICOLON',
    'ASSIGN',
    'NUM',
    'PUNTO',
    'LPAREN',
    'RPAREN',
    'STRING',
    'CONSOLE',
    'LOG',
    'OPERADOR',
    'IF',
    'ELSE',
    'LBRACE',
    'RBRACE',
    'EQUAL',     # ==
    'LESS',      # <
    'GREATER',   # >
    'LESSEQUAL', # <=
    'GREATEQUAL',# >=
    'NOTEQUAL',   # !=
    'AND',
    'OR',
    'FOR',
    'PLUSPLUS',
    'MINUSMINUS'
)

# Definición de reglas
t_OPERADOR = r'\+|-|\*|/'
t_EQUAL = r'=='
t_LESS = r'<'
t_GREATER = r'>'
t_LESSEQUAL = r'<='
t_GREATEQUAL = r'>='
t_NOTEQUAL = r'!='
t_ASSIGN = r'='
t_AND = r'&&'
t_OR = r'\|\|'
t_SEMICOLON = r';'
t_LBRACE =r'\{'
t_RBRACE = r'\}'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_STRING = r'"[^"]*"'
t_PUNTO = r'\.'

def t_LET(t):
    r'let'
    return t

def t_VAR(t):
    r'var'
    return t

def t_CONST(t):
    r'const'
    return t

def t_CONSOLE(t):
    r'console'
    return t

def t_LOG(t):
    r'log'
    return t

def t_IF(t):
    r'if'
    return t

def t_ELSE(t):
    r'else'
    return t

def t_FOR(t):
    r'for'
    return t

def t_MINUSMINUS(t):
    r'--'
    return t

def t_PLUSPLUS(t):
    r'\+\+'
    return t

def t_NUM(t):
    r'\d+(\.\d+)?'
    if t.value.isdigit():
        t.value = int(t.value)
    else:
        t.value = float(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    #if t.value not in symbol_table:
    #       symbol_table[t.value] = {'type': t.type, 'line': t.lineno}
    return t


# Reglas para ignorar caracteres como espacios y tabulaciones
t_ignore = ' \t'

# Regla para manejar saltos de línea
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Regla para manejar errores
def t_error(t):
    print(f"Error: Carácter inesperado '{t.value[0]}' en la línea {t.lineno}")
    t.lexer.skip(1)

# Crear el analizador léxico
lexer = lex.lex()

def imprimir_tabla_simbolos():
    print("\nTabla de Símbolos:")
    print("-----------------")
    print(f"{'ID': <15}{'Tipo': <10}{'Línea'}")
    print("-----------------")
    
    for identificador, info in symbol_table.items():
        tipo = info['type']
        print(f"{identificador}\t{tipo}")

# Ejemplo de uso
# Prueba del analizador
with open('codigo.txt', 'r') as file:
    source_code = file.read()

# Configurar el analizador léxico con el código fuente
lexer.input(source_code)

# Tokenize e impresión de resultados
while True:
    tok = lexer.token()
    if not tok:
        break  # No hay más tokens
    print(tok)
lexer.lineno = 1
class Nodo():
    def __init__(self, ident=0):
        self.ident = ident
class Programa(Nodo):
    def __init__(self, declaraciones):
        self.declaraciones = declaraciones

    def parseToPython(self,identation=0):
        if self.declaraciones is not None:
            return self.declaraciones.parseToPython(identation)
        else:
            return ""

class Declaraciones(Nodo):
    def __init__(self, declaraciones, declaracion):
        self.declaraciones = declaraciones
        self.declaracion = declaracion

    def parseToPython(self, indentation=0):
        result = ""
        if self.declaraciones is not None:
            result += f'{self.declaraciones.parseToPython(indentation)}'
        if self.declaracion is not None:
            result += f'\n{self.declaracion.parseToPython(indentation)}'
        return result
class DeclaracionesDeclaracion(Nodo):
    def __init__(self, declaracion):
        self.declaracion = declaracion

    def parseToPython(self,identation=0):
        return f'{self.declaracion.parseToPython(identation)}'
class Declaracion(Nodo):
    def __init__(self, declaracion):
        self.declaracion = declaracion

    def parseToPython(self,identation=0):
        
        ind=" "*identation
        return f'{ind}{self.declaracion.parseToPython(identation)}'
class Asignacion(Nodo):
    def __init__(self, val_name, operador, son1):
        self.val = val_name
        self.operador = operador
        self.son1 = son1

    def parseToPython(self,identation=0):
        if self.val not in symbol_table:
            tipoDeVariable = self.son1.tipo()
            print(tipoDeVariable)
            symbol_table[self.val] = {'type': tipoDeVariable}

            # Verificar si la asignación es una cadena y actualizar el tipo en la tabla de símbolos
            if tipoDeVariable == 'String':
                symbol_table[self.val]['type'] = 'String'

        return f"{self.val} {self.operador} {self.son1.parseToPython(identation)}"
    def id(self):
        return self.val
    def valor(self):
        return self.son1.parseToPython(0)
class Expresion(Nodo):
    def __init__(self, val_name):
        self.val = val_name

    def parseToPython(self,identation=0):
        return self.val
    def tipo(self):
        return self.val.tipo()
class ExpresionIdentificador(Nodo):
    def __init__(self, val_name,lineno):
        self.val = val_name
        self.lineno = lineno
    def parseToPython(self,identation=0):
       #print(self.val in symbol_table)
        if self.val in symbol_table:
            return self.val 
        else:
            #print('Entrando a else')
            tabla_errores.append(f'Error en la linea {self.lineno} ')
            #print(len(tabla_errores))
    def tipo(self):
        if self.val in symbol_table:
            return symbol_table[self.val].get('type')
        else:
            return f'Variable'
class ExpresionIncDec(Nodo):
    def __init__(self, identificador, operador):
        self.identificador = identificador
        self.operador = operador
    def parseToPython(self,identation=0):
        if self.operador == '++' and self.identificador in symbol_table:
            return f'{self.identificador}+=1'
        elif self.operador == '--' and self.identificador in symbol_table:
            return f'{self.identificador}-=1'
        return f'XD'
class ExpresionMultiple(Nodo):
    def __init__(self, expresion_1, operador, expresion_2):
        self.expresion_1 = expresion_1
        self.expresion_2 = expresion_2
        self.operador = operador

    def parseToPython(self,identation=0):
        #print(self.expresion_1.tipo())
        
        if self.operador == '+' and (self.expresion_1.tipo() == 'String' or self.expresion_2.tipo() == 'String'):
            if self.expresion_1.tipo() == 'Number' and self.expresion_2.tipo() == 'String':
                return f'str({self.expresion_1.parseToPython(identation)}) {self.operador} {self.expresion_2.parseToPython(identation)}'
            elif self.expresion_1.tipo() == 'String' and self.expresion_2.tipo() == 'Number':
                return f'{self.expresion_1.parseToPython(identation)} {self.operador} str({self.expresion_2.parseToPython(identation)})'
            else:
                return f'{self.expresion_1.parseToPython(identation)} {self.operador} {self.expresion_2.parseToPython(identation)}'

        return f'{self.expresion_1.parseToPython(identation)} {self.operador} {self.expresion_2.parseToPython(identation)}'
    def tipo(self):
        tipoExpresion_1 = self.expresion_1.tipo()
        tipoExpresion_2 = self.expresion_2.tipo()

        if self.operador == '+' and (tipoExpresion_1 == 'String' or tipoExpresion_2 == 'String'):
            return 'String'
        elif tipoExpresion_1 != tipoExpresion_2:
            # Manejar otro caso si los tipos no son iguales
            print('')
        else:
            return tipoExpresion_1
class Valor(Nodo):
    def __init__(self, valor):
        self.valor = valor
    def parseToPython(self):
        return self.valor
    def __str__(self):
        return str(self.valor)
    def tipo(self):
        if self.valor is False or self.valor is True:
            return 'Bool'
        elif isinstance(self.valor, int) or isinstance(self.valor, float):
            return 'Number'
        elif isinstance(self.valor, str):
            return 'String'
        else:
            return 'None'
class Mostrar(Nodo):
    def __init__(self, expresion):
        self.expresion = expresion
    def parseToPython(self,identation=0):
        return f'print({self.expresion.parseToPython(identation)})'

class CondicionalIf(Nodo):
    def __init__(self, comparacion, declaraciones, elif_blocks=None, else_block=None):
        self.comparacion = comparacion
        self.declaraciones = declaraciones
        self.elif_blocks = elif_blocks
        self.else_block = else_block

    def parseToPython(self, indentation=0):
        ind = " " * indentation
        result = f'if {self.comparacion.parseToPython()}:'
        result += f'\n{self.declaraciones.parseToPython(indentation+4)}'
        if self.elif_blocks:
            result += f'\n{self.elif_blocks.parseToPython(indentation)}'
        
        if self.else_block:
            result += f'\n{self.else_block.parseToPython(indentation)}'

        return result

class CondicionalesEliF(Nodo):
    def __init__(self, elif_s, el_if):
        self.elif_s = elif_s
        self.el_if = el_if

    def parseToPython(self, indentation=0):
        result = ""
        if self.elif_s is not None:
            result += f'{self.elif_s.parseToPython(indentation)}\n'
        if self.el_if is not None:
            result += f'{self.el_if.parseToPython(indentation)}'
        return result

class Comparacion(Nodo):
    def __init__(self,expresion1,comparador,expresion2):
        self.expresion1=expresion1
        self.comparador=comparador
        self.expresion2=expresion2
    def parseToPython(self):
        if self.comparador=='&&':
            return f'{self.expresion1.parseToPython()} and {self.expresion2.parseToPython()}'
        elif self.comparador=='||':
            return f'{self.expresion1.parseToPython()} or {self.expresion2.parseToPython()}'
        return f'{self.expresion1.parseToPython()} {self.comparador} {self.expresion2.parseToPython()}'
    def der(self):
        
        print(self.expresion2.parseToPython())
        return self.expresion2.parseToPython()
class CondicionalElif(Nodo):
    def __init__(self, comparacion, declaraciones):
        self.comparacion = comparacion
        self.declaraciones = declaraciones

    def parseToPython(self, indentation=0):
        ind = " " * indentation
        result = f'{ind}elif {self.comparacion.parseToPython()}:'
        result += f'\n{self.declaraciones.parseToPython(indentation+4)}'
        return result

class CondicionalElse(Nodo):
    def __init__(self, declaraciones):
        self.declaraciones = declaraciones

    def parseToPython(self, indentation=0):
        ind = " " * indentation
        result = f'{ind}else:'
        result += f'\n{self.declaraciones.parseToPython(indentation+4)}'
        return result

class BucleFor(Nodo):
    def __init__(self, declaracion_inicial, comparacion, declaracion_final, declaraciones):
        self.declaracion_inicial = declaracion_inicial
        self.comparacion = comparacion
        self.declaracion_final = declaracion_final
        self.declaraciones = declaraciones
    def parseToPython(self, indentation=0):
        print(self.declaracion_final.parseToPython(indentation))
        ind = " " * indentation
        result = f'{ind}for {self.declaracion_inicial.id()} in range({self.comparacion.der()}, {self.declaracion_inicial.valor()}):'
        result += f'\n{self.declaraciones.parseToPython(indentation+4)}'
        
        return result

precedence = (
    ('left', 'OPERADOR'),
    ('left', 'EQUAL', 'LESS', 'GREATER', 'LESSEQUAL', 'GREATEQUAL', 'NOTEQUAL'),
    ('left', 'OR'),
    ('left', 'AND'),
)
    
def p_programa(p):
    '''
    programa : declaraciones
            | empty
    '''
    p[0]=Programa(p[1])
    
def p_declaraciones(p):
    '''
    declaraciones : declaraciones declaracion
    '''
    if len(p)== 4:
        p[0]=Declaraciones(p[1],p[3])
    elif len(p) == 3:
        p[0]=Declaraciones(p[1],p[2]) 
def p_declaracionesDeclaracion(p):
    '''
    declaraciones : declaracion
    '''
    p[0]=DeclaracionesDeclaracion(p[1])
def p_declaracion(p):
    '''
    declaracion : asignacion
                | mostrar
                | condicional
                | bucle_for
                | unario
    '''
    p[0]=Declaracion(p[1])
def p_asignacion(p):
    '''
    asignacion : LET ID ASSIGN expresion
               | VAR ID ASSIGN expresion
               | CONST ID ASSIGN expresion
    '''
    p[0]=Asignacion(p[2],p[3],p[4])
def p_mostrar(p):
    '''
    mostrar : CONSOLE PUNTO LOG LPAREN expresion RPAREN
    '''
    p[0] = Mostrar(p[5])
def p_expresion(p):
    '''
    expresion : valor
    '''
    p[0] = Expresion(p[1])

def p_expresionIdentificador(p):
    '''
    expresion : ID
    '''
    p[0] = ExpresionIdentificador(p[1], p.lineno(1))
    
def p_unario(p):
    '''
    unario : ID MINUSMINUS
           | ID PLUSPLUS
    '''
    p[0] = ExpresionIncDec(p[1],p[2])
    
def p_expresionMultiple(p):
    '''
    expresion : expresion OPERADOR expresion
    '''
    p[0]=ExpresionMultiple(p[1],p[2],p[3])
def p_valor(p):
    '''
    valor : NUM
          | STRING
    '''
    p[0]=Valor(p[1])
    
def p_condicional(p):
    '''
    condicional : IF LPAREN comparacion RPAREN LBRACE declaraciones RBRACE else_if_blocks else_block
    '''
    p[0] = CondicionalIf(p[3], p[6], p[8], p[9])
    
def p_else_block(p):
    '''
    else_block : ELSE LBRACE declaraciones RBRACE
                | empty
    '''
    if len(p) == 5:
        p[0] = CondicionalElse(p[3])

def p_else_if_blocks(p):
    '''
    else_if_blocks : empty
                   | else_if_blocks else_if_block
    '''
    if len(p) == 3:
        p[0] = CondicionalesEliF(p[1],p[2])

def p_else_if_block(p):
    '''
    else_if_block : ELSE IF LPAREN comparacion RPAREN LBRACE declaraciones RBRACE
    '''
    p[0] = CondicionalElif(p[4], p[7])
    
def p_comparacion(p):
    '''
    comparacion : expresion EQUAL expresion
                | expresion LESS expresion
                | expresion GREATER expresion
                | expresion LESSEQUAL expresion
                | expresion GREATEQUAL expresion
                | expresion NOTEQUAL expresion
                | comparacion OR comparacion
                | comparacion AND comparacion
    '''
    p[0] = Comparacion(p[1], p[2], p[3])
    
def p_bucle_for(p):
    '''
    bucle_for : FOR LPAREN asignacion SEMICOLON comparacion SEMICOLON unario RPAREN LBRACE declaraciones RBRACE
    '''
    p[0] = BucleFor(p[3],p[5],p[7],p[10])

def p_empty(p):
    'empty :'
    pass
def p_error(p):
    if p is not None:
        tabla_errores.append(f'Eror sitanctico en la linea {p.lineno} ')
    
parser = yacc.yacc()

# Parsear e imprimir resultados
result = parser.parse(source_code,tracking=True)

if result is not None:
    print("Codigo Fuente:\n")
    print(source_code)
    # Guardar el código Python en un archivo con nombre traducido
    nombre_archivo_traducido = 'codigo_traducido.py'
    source_Python=result.parseToPython()
    #print(len(tabla_errores))
    if len(tabla_errores) > 0:
        for valor in tabla_errores:
            print(f'{valor}')
    else:
        print("Compilacion de codigo: ")
        print(source_Python)
        with open(nombre_archivo_traducido, 'w') as file:
            file.write(source_Python)
        print(f"\nSe ha creado el archivo '{nombre_archivo_traducido}' con el código traducido.")
        imprimir_tabla_simbolos()