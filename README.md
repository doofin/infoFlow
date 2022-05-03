# 02244 Logic For Security Spring 2022 at DTU


# Run

first you need to install java and sbt(https://www.scala-sbt.org/)


then cd into this folder and in command line type (might take ~5 min):
```sbt run```

to test Denning's and Meyer's approach

# output msg

the output message like :

`test case 0: testdata1: implicit flow in if block, a->b (should be false), result: false`


means that test case 0 should be false (because of violation of info flow ),and the  result is false,matching expection.


`! constraint failed at: If_(opR(Var(a),intValue(11),<=),Var(b) := intValue(10),Var(a) := intValue(100))`


means that it fails at this specific program statement

# Usage

View sec.2 of our report 

Denning's approach:
- set the initial variables map, true for HIGH,false for LOW
- state the statement list including the IFs and WHILEs
- return the test description, the variables map and the statements list


Meyer's approach: View sec.2 of our report 

# Develop Requirement
- sbt
- IDE: vscode + metals


# Project on Information Flow
Contributors:
- Yuchen Du (s212624@student.dtu.dk)
- Mário Gažo (s212687@student.dtu.dk)
- Filip Yankov (s210269@student.dtu.dk)

