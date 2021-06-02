IDENTIFICATION DIVISION.
PROGRAM-ID. REPASO-3.
DATA DIVISION.
FILE SECTION.

WORKING-STORAGE SECTION.

01  OPC-1           PIC     99.
01  OPC-2           PIC     XX       VALUE   SPACE.
01  OPC-3           PIC     XX       VALUE   SPACE.
01  OPC-4           PIC     99       VALUE   ZERO.
01  NUMBER-1        PIC     S9(4)    VALUE   ZERO.
01  NUMBER-2        PIC     S9(4)    VALUE   ZERO.
01  RES-ADD         PIC     S9(5).
01  RES-SUBTRACT    PIC     S9(5).
01  RES-MULTIPLY    PIC     S9(5).
01  RES-DIVIDE      PIC     S9(5).
01  RES-REMAINDER   PIC     S9(5).
01  NUMBER-INIT     PIC     9(6)     VALUE   ZERO.   
01  MAX-VALUE       PIC     9(6)     VALUE   ZERO.
01  NUMBER-RANGE    PIC     9(6)     VALUE   ZERO.
01  ITERATOR-1      PIC     9999     VALUE   ZERO.
01  ITERATOR-2      PIC     9999     VALUE   ZERO.
01  ITERATOR-3      PIC     9(5)     VALUE   ZERO.
01  ITERATOR-4      PIC     9999     VALUE   ZERO.

PROCEDURE DIVISION.

    MENU-PROGRAMA.
        DISPLAY "..::MENU PROGRAMA::..".
        DISPLAY "1) Operaciones basicas.".
        DISPLAY "2) Iteraciones.".
        DISPLAY "0) Salir.".
        DISPLAY "Ingresa la opcion que deseas seleccionar: ".
        ACCEPT OPC-1.
        
        IF OPC-1 = 1
            PERFORM SUBP-1-CALCULADORA
        ELSE
            IF OPC-1 = 2
                PERFORM SUBP-2-TABLAS
            ELSE 
                IF OPC-1 = 0
                    PERFORM FINALIZAR
                ELSE
                    DISPLAY "Opcion no valida."
                    PERFORM MENU-PROGRAMA
                END-IF
            END-IF
        END-IF.
        
    REINICIAR-VALORES.
        MOVE 0 TO NUMBER-1 NUMBER-2.
    
    CONTINUAR.
        DISPLAY "Deseas continuar (escribe SI o NO): ".
        ACCEPT OPC-3.
        
        IF OPC-3 = "SI" OR OPC-3 = "si"
            PERFORM MENU-PROGRAMA
        ELSE
            PERFORM FINALIZAR.
    
    FINALIZAR.
        STOP RUN.    
    
    SUBP-MENU-1.
        DISPLAY "..::SUB MENU CALCULADORA::..".
        DISPLAY "A) SUMA.".
        DISPLAY "B) RESTA.".
        DISPLAY "C) MULTIPLICACION.".
        DISPLAY "D) DIVISION Y RESTO.".
        DISPLAY "0) SALIR.".
        DISPLAY "Ingresa la opcion: ".
        ACCEPT OPC-2.
        
        IF OPC-2 = "A" OR OPC-2 = "a"
            PERFORM SUBP-1-INGRESAR
            PERFORM SUBP-1-ADD
        ELSE
            DISPLAY " ".
        
        IF OPC-2 = "B" OR OPC-2 = "b"
            PERFORM SUBP-1-INGRESAR
            PERFORM SUBP-1-SUBTRACT
        ELSE
            DISPLAY " ".
            
        IF OPC-2 = "C" OR OPC-2 = "c"
            PERFORM SUBP-1-INGRESAR
            PERFORM SUBP-1-MULTIPLY
        ELSE
            DISPLAY " ".
            
        IF OPC-2 = "D" OR OPC-2 = "d"
            PERFORM SUBP-1-INGRESAR
            PERFORM SUBP-1-DIVIDE
        ELSE
            DISPLAY " ".
            
        IF OPC-2 = "0"
            PERFORM MENU-PROGRAMA
        ELSE
            DISPLAY " ".
            
        PERFORM CONTINUAR.
        
    SUBP-1-INGRESAR.
        DISPLAY "Ingresa el primer valor: ".
        ACCEPT NUMBER-1.
        DISPLAY "Ingresa el segudno valor: ".
        ACCEPT NUMBER-2.
        
    SUBP-1-ADD.
        ADD NUMBER-1 TO NUMBER-2 GIVING RES-ADD.
        DISPLAY NUMBER-1 " + " NUMBER-2 " = " RES-ADD.
    
    SUBP-1-SUBTRACT.
        SUBTRACT NUMBER-1 FROM NUMBER-2 GIVING RES-SUBTRACT.
        DISPLAY NUMBER-1 " - " NUMBER-2 " = " RES-SUBTRACT.
    
    SUBP-1-MULTIPLY.
        MULTIPLY NUMBER-1 BY NUMBER-2 GIVING RES-MULTIPLY.
        DISPLAY NUMBER-1 " * " NUMBER-2 " = " RES-MULTIPLY.
    
    SUBP-1-DIVIDE.
        DIVIDE NUMBER-1 BY NUMBER-2 GIVING RES-DIVIDE REMAINDER RES-REMAINDER.
        DISPLAY NUMBER-1 " / " NUMBER-2 " = " RES-DIVIDE.
        DISPLAY NUMBER-1 " % " NUMBER-2 " = " RES-REMAINDER.
    
    SUBP-2-INGRESAR.
        DISPLAY "Ingresa el inicio de la iteracion: ".
        ACCEPT NUMBER-INIT.
        DISPLAY "Ingresa el limite a iterar.".
        ACCEPT MAX-VALUE.
        DISPLAY "Ingresa el rango de iteracion.".
        ACCEPT NUMBER-RANGE.
    
    SUBP-MENU-2.
        DISPLAY "..::SUB MENU TABLAS DE MULTIPLICAR::..".
        DISPLAY "1) INCREMENTAR ++/+=".
        DISPLAY "2) DECREMENTAR --/-=".
        DISPLAY "3) INCREMENTAR *=".
        DISPLAY "4) DECREMENTAR /=".
        DISPLAY "0) SALIR."
        DISPLAY "Como quieres iterar: ".
        ACCEPT OPC-4.
        
        IF OPC-4 = 1
            PERFORM SUBP-2-INGRESAR
            PERFORM SUBP-2-INCREMENTAR-ADD
        ELSE
            DISPLAY " ".
            
        IF OPC-4 = 2
            PERFORM SUBP-2-INGRESAR
            PERFORM SUBP-2-DECREMENTAR-SUBTRACT
        ELSE
            DISPLAY " ".
            
        IF OPC-4 = 3
            PERFORM SUBP-2-INGRESAR
            PERFORM SUBP-2-INCREMENTAR-MULTIPLY
        ELSE
            DISPLAY " ".
            
        IF OPC-4 = 4
            PERFORM SUBP-2-INGRESAR
            PERFORM SUBP-2-DECREMENTAR-DIVIDE
        ELSE
            DISPLAY " ".
            
        IF OPC-4 = 0
            PERFORM MENU-PROGRAMA
        ELSE
            DISPLAY " ".
            
        PERFORM CONTINUAR.
            
    SUBP-2-INCREMENTAR-ADD.
        PERFORM VARYING NUMBER-INIT FROM NUMBER-INIT BY NUMBER-RANGE UNTIL NUMBER-INIT > MAX-VALUE
        DISPLAY NUMBER-INIT
        END-PERFORM.
    
    SUBP-2-DECREMENTAR-SUBTRACT.
        PERFORM SUBP-2-DECREMENTAR-SUBTRACT-OPERATION MAX-VALUE TIMES.
        
    SUBP-2-DECREMENTAR-SUBTRACT-OPERATION.
        SUBTRACT NUMBER-RANGE FROM NUMBER-INIT.
        DISPLAY NUMBER-INIT.
    
    SUBP-2-INCREMENTAR-MULTIPLY.
        PERFORM SUBP-2-INCREMENTAR-MULTIPLY-OPERATION MAX-VALUE TIMES.
    
    SUBP-2-INCREMENTAR-MULTIPLY-OPERATION.
        MULTIPLY NUMBER-RANGE BY NUMBER-INIT.
        DISPLAY NUMBER-INIT.
    
    SUBP-2-DECREMENTAR-DIVIDE.
        PERFORM SUBP-2-DECREMENTAR-DIVIDE-OPERATION MAX-VALUE TIMES.
    
    SUBP-2-DECREMENTAR-DIVIDE-OPERATION.
        DIVIDE NUMBER-RANGE INTO NUMBER-INIT.
        DISPLAY NUMBER-INIT.
    
    SUBP-1-CALCULADORA.
        PERFORM SUBP-MENU-1.
    
    SUBP-2-TABLAS.
        PERFORM SUBP-MENU-2.

END PROGRAM REPASO-3.
