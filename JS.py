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
    'MINUSMINUS',
    'LBRACKET',
    'RBRACKET',
    'COMMA',
    'COLON',
    'WHILE',
    'PLUSEQ',
    'MINUSEQ',
    'TIMESEQ',
    'DIVEQ',
    'POWEQ',
    'LENGTH',
    'FUNCTION'
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
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COLON = r'\:'
t_COMMA = r'\,'
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

def t_LENGTH(t):
    r'length'
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

def t_WHILE(t):
    r'while'
    return t

def t_FUNCTION(t):
    r'function'
    return t

def t_MINUSMINUS(t):
    r'--'
    return t

def t_PLUSPLUS(t):
    r'\+\+'
    return t

def t_PLUSEQ(t):
    r'\+='
    return t

def t_MINUSEQ(t):
    r'\-='
    return t

def t_TIMESEQ(t):
    r'\*='
    return t

def t_DIVEQ(t):
    r'/='
    return t
    
def t_POWEQ(t):
    r'\*\*='
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

def t_COMMENT(t):
    r'//.*'
    pass

# Reglas para ignorar caracteres como espacios y tabulaciones
t_ignore = ' \t'

# Regla para manejar saltos de línea
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Regla para manejar errores
def t_error(t):
    tabla_errores.append(f"Error: Carácter inesperado '{t.value[0]}' en la línea {t.lineno}")
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
with open('script.js', 'r') as file:
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

def tipo(valor):
    if valor is False or valor is True:
        return 'Bool'
    elif isinstance(valor, int) or isinstance(valor, float):
        return 'Number'
    elif isinstance(valor, str):
        return 'String'
    else:
        return 'None'

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
            print(self.son1)
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
    
class AsignacionDespuesDeDeclaracion(Nodo):
    def __init__(self, identificador, operador, expresion):
        self.identificador = identificador
        self.operador = operador
        self.expresion = expresion
    
    def parseToPython(self, identation = 0):
        if self.identificador in symbol_table:
            symbol_table[self.identificador]['type'] = self.expresion.tipo()
            return f'{self.identificador} {self.operador} {self.expresion.parseToPython()}'
        else:
           tabla_errores.append(f'Error: variable {self.identificador} no declarada ')      
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

#Aignacion Multiple

class AsignacionMultiple(Nodo):
    def __init__(self, variables_assign):
        self.variables_assign = variables_assign
    def parseToPython(self, identation = 0):
        return self.variables_assign.parseToPython()

class VariablesAssign(Nodo):
    def __init__(self, identificador, variables_assign = None, expresion = None):
        self.identificador = identificador
        self.variables_assign = variables_assign
        self.expresion = expresion
    def parseToPython(self, identation = 0):
        if self.variables_assign is None and self.expresion is None:
            if self.identificador not in symbol_table:
                symbol_table[self.identificador] = {'type': 'None'}
            return f'{self.identificador} = None'
        elif self.expresion is None:
            if self.identificador not in symbol_table:
                symbol_table[self.identificador] = {'type': 'None'}
            return f'{self.variables_assign.parseToPython()}\n{self.identificador} = None'
        elif self.variables_assign is None:
            if self.identificador not in symbol_table:
                symbol_table[self.identificador] = {'type': self.expresion.tipo()}
            return f'{self.identificador} = {self.expresion.parseToPython()}'
        else:
            if self.identificador not in symbol_table:
                symbol_table[self.identificador] = {'type': self.expresion.tipo()}
            return f'{self.variables_assign.parseToPython()} \n{self.identificador} = {self.expresion.parseToPython()}'

#Array        

class ExpresionArray(Nodo):
    def __init__(self, elementos = None):
        self.elementos = elementos
    def parseToPython(self,identation = 0):
        if self.elementos is not None:
            return f'[{self.elementos.parseToPython()}]'
        else:
            return '[]'
    def tipo(self):
        return self.elementos.tipo()
class Elementos(Nodo):
    def __init__(self,elemento,elementos=None):
        self.elemento = elemento
        self.elementos = elementos
    def parseToPython(self,identtation = 0):
        if self.elementos is None:
            return f'{self.elemento.parseToPython()}'
        else:
            return f'{self.elemento.parseToPython()},{self.elementos.parseToPython()}'
    def tipo(self):
        return self.elemento.tipo()
class ElementoArray(Nodo):
    def __init__(self,identificador, indice):
        self.identificador = identificador
        self.indice = indice
    def parseToPython(self, indentation = 0):
        return f'{self.identificador}[{self.indice}]'
    def tipo(self):
        return symbol_table[self.identificador].get('type')
    
class LongitudArray(Nodo):
    def __init__(self, identificador):
        self.identificador = identificador
    def parseToPython(self, identation = 0):
        return f'len({self.identificador})'
    def tipo(self):
        return 'Number'
#Array de objetos

class ObjetosArray(Nodo):
    def __init__(self, objetos = None):
        self.objetos = objetos
    def parseToPython(self, indetation = 0):
        if self.objetos is not None:
            return f'[{self.objetos.parseToPython()}]'
        else:
            return '[{}]'    
    def tipo(self):
        return 'Array'

class Objetos(Nodo):
    def __init__(self, objeto, objetos = None):
        self.objeto = objeto
        self.objetos = objetos
    def parseToPython(self, indetation = 0):
        if self.objetos is None:
            return f'{self.objeto.parseToPython()}'
        else:
            return f'{self.objeto.parseToPython()},\n{self.objetos.parseToPython()}'
class Objeto(Nodo):
    def __init__(self, propiedades):
        self.propiedades = propiedades
    def parseToPython(self, identation = 0):
        return f'{{{self.propiedades.parseToPython()}}}'
class Propiedades(Nodo):
    def __init__(self, propiedad, propiedades = None):
        self.propiedad = propiedad
        self.propiedades = propiedades
    def parseToPython(self, identation = 0):
        if self.propiedades is None:
            return f'{self.propiedad.parseToPython()}'
        else:
            return f'{self.propiedad.parseToPython()},{self.propiedades.parseToPython()}'
class Propiedad(Nodo):
    def __init__(self, identificador, valor):
        self.identificador = identificador
        self.valor = valor
    def parseToPython(self, identation = 0):
        tipoDeVariable = self.valor.tipo()
        print(tipoDeVariable)
        symbol_table[self.identificador] = {'type': tipoDeVariable}
        
        return f'\'{self.identificador}\': {self.valor}'
        
class ExpresionAccesoObjetoArray(Nodo):
    def __init__(self, identificador_array, indice, identificador_propiedad):
        self.identificador_array = identificador_array
        self.indice = indice
        self.identificador_propiedad = identificador_propiedad
    def parseToPython(self, identation = 0):
        return f'{self.identificador_array}[{self.indice}][\'{self.identificador_propiedad}\']'
    def tipo(self):
        return symbol_table[self.identificador_propiedad].get('type')
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
        ind = " " * indentation
        if self.declaracion_inicial.id() not in symbol_table:
            symbol_table[self.declaracion_inicial.id()] = {'type': 'Number'}
        result = f'{ind}for {self.declaracion_inicial.id()} in range({self.declaracion_inicial.valor()}, {self.comparacion.der()}):'
        result += f'\n{self.declaraciones.parseToPython(indentation+4)}'
        
        return result

class BucleWhile(Nodo):
    def __init__(self, comparacion, declaraciones):
        self.comparacion = comparacion
        self.declaraciones = declaraciones
    def parseToPython(self, identation = 0):
        ind = " "*identation
        result = f'{ind}while {self.comparacion.parseToPython()}:'
        result += f'\n{self.declaraciones.parseToPython(identation+4)}'
        
        return result

class Funcion(Nodo):
    def __init__(self, identificador, parametros, declaraciones):
        self.identificador = identificador
        self.parametros = parametros
        self.declaraciones = declaraciones
    def parseToPython(self, identation = 0):
        ind = " "*identation
        result = f'def {self.identificador} ({self.parametros.parseToPython()}):'
        result+=f'\n{self.declaraciones.parseToPython(identation + 4)}'
        
        return result
    
class Parametros(Nodo):
    def __init__(self,parametro=None, parametros = None):
        self.parametro = parametro
        self.parametros = parametros
    def parseToPython(self, identation = 0):
        if self.parametro is not None:
            if self.parametros is not None:
                return f'{self.parametro.parseToPython()}, {self.parametros.parseToPython()}'
            else:
                return f'{self.parametro.parseToPython()}'
        else:
            return ''
class Parametro(Nodo):
    def __init__(self, nombre):
        self.nombre = nombre

    def parseToPython(self):
        if self.nombre not in symbol_table:
            symbol_table[self.nombre] = {'type': 'None'}
        return self.nombre
        
class LlamadaFuncionS(Nodo):
    def __init__(self, nombre, argumentos=None):
        self.nombre = nombre
        self.argumentos = argumentos

    def parseToPython(self, identation=0):
        return f'{self.nombre}({self.argumentos.parseToPython()})'
        
class LlamadaFuncion(Nodo):
    def __init__(self, llamada_funcion):
        self.llamada_funcion = llamada_funcion
    def parseToPython(self, identation=0):
        return self.llamada_funcion.parseToPython()

class Argumentos(Nodo):
    def __init__(self, argumento, argumentos):
        self.argumento = argumento
        self.argumentos = argumentos
    def parseToPython(self, identation = 0):
        if self.argumentos is None:
            return f'{self.argumento.parseToPython()}'
        else:
           return f'{self.argumento.parseToPython()},{self.argumentos.parseToPython()}' 
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
    declaracion : funcion
                | asignacion_multiple
                | asignacion_multiple_sin_semicolon
                | asignacion
                | asignacion_sin_semicolon
                | mostrar
                | condicional
                | bucle_for
                | unario
                | unario_sin_semicolon
                | bucle_while
                | llamada_funcion
    '''
    p[0]=Declaracion(p[1])
def p_asignacion(p):
    '''
    asignacion : LET ID ASSIGN expresion SEMICOLON
               | VAR ID ASSIGN expresion SEMICOLON
               | CONST ID ASSIGN expresion SEMICOLON
               | LET ID ASSIGN expresion_array SEMICOLON
               | CONST ID ASSIGN expresion_array SEMICOLON
               | VAR ID ASSIGN expresion_array SEMICOLON
               | LET ID ASSIGN objeto_array SEMICOLON
               | CONST ID ASSIGN objeto_array SEMICOLON
               | VAR ID ASSIGN objeto_array SEMICOLON
    '''
    p[0] = Asignacion(p[2], p[3], p[4])

def p_asignacion_sin_semicolon(p):
    '''
    asignacion_sin_semicolon : LET ID ASSIGN expresion
                             | VAR ID ASSIGN expresion
                             | CONST ID ASSIGN expresion
                             | LET ID ASSIGN expresion_array
                             | CONST ID ASSIGN expresion_array
                             | VAR ID ASSIGN expresion_array
                             | LET ID ASSIGN objeto_array
                             | CONST ID ASSIGN objeto_array
                             | VAR ID ASSIGN objeto_array
    '''
    p[0] = Asignacion(p[2], p[3], p[4])

    
#Asginacion Multiple    
    
def p_asignacionMultiple(p):
    '''
    asignacion_multiple : LET variables_assign SEMICOLON
                        | VAR variables_assign SEMICOLON
                        | CONST variables_assign SEMICOLON
    '''
    p[0]=AsignacionMultiple(p[2])
def p_asignacionMultipleSinSemiColon(p):
    '''
    asignacion_multiple_sin_semicolon : LET variables_assign
                                      | VAR variables_assign
                                      | CONST variables_assign
    '''
    p[0]=AsignacionMultiple(p[2])
def p_variablesAssign(p):
    '''
    variables_assign : ID
                     | variables_assign COMMA ID ASSIGN expresion
    '''
    if len(p) == 2:
        p[0]=VariablesAssign(p[1])
    elif len(p) == 4:
        p[0] = VariablesAssign(p[1],None,p[3])
    elif len(p) == 6:
        p[0] = VariablesAssign(p[3],p[1],p[5])
def p_variablesAssignVars(p):
    '''
    variables_assign : variables_assign COMMA ID
    '''
    p[0] = VariablesAssign(p[3],p[1])
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

def p_expresionFuncion(p):
    '''
    expresion : llamada_funcion
    '''
    p[0] = LlamadaFuncion(p[1])
def p_expresionArray(p):
    '''
    expresion_array : LBRACKET RBRACKET 
                    | LBRACKET elementos RBRACKET
    '''
    if len(p) == 3:
        p[0]=ExpresionArray()
    elif len(p) == 4:
        p[0]=ExpresionArray(p[2])
def p_elementos(p):
    '''
    elementos : expresion
              | elementos COMMA expresion
    '''
    if len(p) == 2:
        p[0]=Elementos(p[1])
    elif len(p) == 4:
        p[0]=Elementos(p[1],p[3])
        
def p_elementoArray(p):
    '''
    expresion : ID LBRACKET NUM RBRACKET
              | ID LBRACKET ID RBRACKET
    '''
    p[0] = ElementoArray(p[1], p[3])
    
def p_elementoArrayLength(p):
    '''
    expresion : ID PUNTO LENGTH
    '''
    p[0] = LongitudArray(p[1])
def p_objetoArray(p):
    '''
    objeto_array : LBRACKET LBRACE RBRACE RBRACKET
                 | LBRACKET objetos RBRACKET
    '''
    if len(p) == 5:
        p[0] = ObjetosArray()
    elif len(p) == 4:
        p[0] = ObjetosArray(p[2])
def p_objetos(p):
    '''
    objetos : objeto
            | objetos COMMA objeto
    '''
    if len(p) == 2:
        p[0] = Objetos(p[1])
    elif len(p) == 4:
        p[0] = Objetos(p[1],p[3])
def p_objeto(p):
    '''
    objeto : LBRACE propiedades RBRACE
    '''
    p[0] = Objeto(p[2])
def p_propiedades(p):
    '''
    propiedades : propiedad 
                | propiedades COMMA propiedad
    '''
    if len(p) == 2:
        p[0] = Propiedades(p[1])
    elif len(p) == 4:
        p[0] = Propiedades(p[1],p[3])
def p_propiedad(p):
    '''
    propiedad : ID COLON valor
    '''
    p[0] = Propiedad(p[1],p[3])

def p_expresionAccesoObjetoArray(p):
    '''
    expresion : ID LBRACKET NUM RBRACKET PUNTO ID
              | ID LBRACKET ID RBRACKET PUNTO ID
    '''
    p[0] = ExpresionAccesoObjetoArray(p[1], p[3], p[6])

#Por revisar p_unario

def p_unario(p):
    '''
    unario : ID MINUSMINUS SEMICOLON
           | ID PLUSPLUS SEMICOLON
           | ID ASSIGN expresion SEMICOLON
           | ID PLUSEQ expresion SEMICOLON
           | ID MINUSEQ expresion SEMICOLON
           | ID TIMESEQ expresion SEMICOLON
           | ID DIVEQ expresion SEMICOLON
           | ID POWEQ expresion SEMICOLON
    '''
    if len(p) == 4:
        p[0] = ExpresionIncDec(p[1],p[2])
    elif len(p) == 5:
        p[0] = AsignacionDespuesDeDeclaracion(p[1],p[2],p[3])
    
def p_unario_sin_semicolon(p):
    '''
    unario_sin_semicolon : ID MINUSMINUS
           | ID PLUSPLUS
           | ID ASSIGN expresion
           | ID PLUSEQ expresion
           | ID MINUSEQ expresion
           | ID TIMESEQ expresion
           | ID DIVEQ expresion
           | ID POWEQ expresion
    '''    
    if len(p) == 3:
        p[0] = ExpresionIncDec(p[1],p[2])
    elif len(p) == 4:
        p[0] = AsignacionDespuesDeDeclaracion(p[1],p[2],p[3])
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
    bucle_for : FOR LPAREN asignacion comparacion SEMICOLON unario_sin_semicolon RPAREN LBRACE declaraciones RBRACE
    '''
    p[0] = BucleFor(p[3],p[4],p[6],p[9])
def p_bucle_while(p):
    '''
    bucle_while : WHILE LPAREN comparacion RPAREN LBRACE declaraciones RBRACE
    '''
    p[0]=BucleWhile(p[3],p[6])
    
def p_funcion(p):
    '''
    funcion : FUNCTION ID LPAREN parametros RPAREN LBRACE declaraciones RBRACE
    '''
    p[0] = Funcion(p[2], p[4], p[7])

def p_parametros(p):
    '''
    parametros : empty
               | parametro
               | parametros COMMA parametro
    '''
    if len(p) == 2:
        p[0] = Parametros(p[1])
    elif len(p) == 4:
        p[0] = Parametros(p[3], p[1])

def p_parametro(p):
    '''
    parametro : ID
    '''
    p[0] = Parametro(p[1])
    
def p_llamada_funcion(p):
    '''
    llamada_funcion : ID LPAREN argumentos RPAREN
    '''
    p[0] = LlamadaFuncionS(p[1], p[3])
    
def p_argumentos(p):
    '''
    argumentos : expresion
               | argumentos COMMA expresion
    '''
    if len(p) == 2:
        p[0] = Argumentos(p[1],None)
    elif len(p) == 4:
        p[0] = Argumentos(p[3], p[1])
    
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