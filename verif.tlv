\m4_TLV_version 1d: tlx.org
/*
BSD 3-Clause License

Copyright (c) 2020, Steve Hoover
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// ================================================================================
// A TL-Verilog M4 library of macros for verification, logging, and instrumentation
// ================================================================================
// (Similar to verif.vh, but as M4 macros.)

// ##################################
// Single-line macros

// ==========
// Assertions
// ==========

// A shorthand for a concurrent assertion statement (which, of course can infer staging logic).
m4_define(['m4_assert'], ['assert property (@(posedge clk) reset || ($1));'])


// ========
// Printing
// ========
// One-line versions of the corresponding multiline macros:
m4_define(['m4_display'], ['always_ff @(posedge clk) \$display($1);'])
m4_define(['m4_display_if'], ['always_ff @(posedge clk) if ($1) \$display($2);'])

// ##################################
// Multi-line macros


// ========
// Printing
// ========

// A macro for printing every clk cycle.
// Params:
//   args: The arguments for the \$display call (the print string and variables).
\TLV display_if(args)
   \SV_plus
      always_ff @(posedge clk) \$display(args);

// A macro for printing under a condition.
// Params:
//   $cond: The condition under which to display (often the signal of the associated $when condition).
//   args: The arguments for the \$display call (the print string and variables).
\TLV display_if($cond, args)
   \SV_plus
      always_ff @(posedge clk) begin
         if ($cond) \$display(args);
      end
   