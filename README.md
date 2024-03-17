# CS141 Coursework 1 2022-23: Bean's Gambit
## A Strategy Game for Two Players

^^ maybe a bit outdated! Try:

# CS141 Coursework 2 2024: Hurtle: The Haskell Logo Turtle
## A language for drawing cool polygon art.

# Language rules
Specifying in more detail some syntactical rules:
 - Commas are treated as newlines
 - A HogoCode is a command that changes the active state of the program, i.e.
    - A movement or rotation (forward, back, right, left, home)
    - Pen command (setwidth, setcolor, penup, pendown, clearscreen)
    - Making a variable (make "<varname> <variable>)
    - Control flow calls:
        - Repeat: repeat <count> [ <listofcode> ]
        - For : for [<counter> <start> <end> <step>] [ <listofcode> ]
        - Procedure calls : <procname> <args>
 - Multiple spaces and multiple newlines are parsed as a single space
 - HogoCode elements must end in a newline
 - Procedure declarations are not HogoCodes, they enclose HogoPrograms and only access what's in their scope
 - They are declared as: "to <procname> :<arg1> :<arg2> [ <listofcode> ] end" with an arbirary number of parameters.
 - Code is case-insensitive
 - A variable can be a float or a reference to another variable, or an operation performed on multiple variables
 - To use a variable, we can say for example: forward :<varname>
 - We can perform arithmetic via keywords (sum, multiply, divide, difference)
 - In the fashion: <opname> <var1> <var2>



## Fail cases
The program isn't perfect (and neither is Logo Turtle, this mimics that in a sense)
 - If you write a program that won't terminate, the code is guaranteed to never run.
 - The interpreter relies on knowing exactly how many calls are made as loops are replaced with the code equivalent; i.e. writing repeat 2 <code> will replace that HogoCode with <code> <code>. A similar process is applied to for loops
 - The conditional for loop is not affected by change of variable inside of the loop, i.e. if you set step to 1 and add 1 to the counter variable inside of the loop, it won't loop fewer times.