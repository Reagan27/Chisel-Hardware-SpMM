#Sparse Matrix Multiplication (SpMM) using Chisel

This project implements a Sparse Matrix Multiplication (SpMM) hardware accelerator using Chisel, a hardware construction language embedded in Scala.

#Overview

Sparse matrix multiplication is a fundamental operation in many scientific and engineering applications. In this project, we implement a hardware accelerator for SpMM, targeting FPGA-based platforms.

The SpMM accelerator takes two input matrices: Matrix A (sparse) and Matrix B (dense, transposed). It computes the product of these matrices and produces the result as Matrix C (dense, transposed).

#Modules
SpMM Module
The SpMM module is the main hardware accelerator for sparse matrix multiplication. It has the following IO ports:

Inputs for Matrix A (sparse): RowIdx, Col, and Data.
Inputs for Matrix B (dense, transposed): RhsReset and Data.
Outputs for Matrix C (dense, transposed): Valid and Data.
The module performs the following operations:

Process the input Matrix B.
Perform sparse matrix multiplication.
Output the result Matrix C.
SpMMTest
The SpMMTest module contains unit tests for the SpMM module. It verifies that the SpMM accelerator produces correct results for a given set of input matrices.

#Usage
To run the tests:
sbt test

#Dependencies
Chisel: A hardware construction language embedded in Scala.
ChiselTest: A testing framework for Chisel.
ScalaTest: A testing tool for Scala.
Contributors
Lutta Reagan 
+254768124427
luttareagan22@gmail.com
