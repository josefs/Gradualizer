---
title: The Gradualizer
author: Josef Svenningsson
date: A long time ago, in a galaxy far, far away...
header-includes: |
    \usepackage{proof}
---

# Introduction

This document describes an new gradual type system for Erlang, and The
Gradualizer, which is a tool which checks types for Erlang programs.

# Type System

Type rules as if Erlang was typed

The judgment is somewhat non-standard. It consists of four parts,
$\Gamma, E , T$ and $\Delta$. $\Gamma$ is the environment and contains
the type of the free variables and functions. $E$ and $T$ are the
expression and its type respectively. $\Delta$ is the variables bound
in the expression. $\Gamma$ and $E$ should be seen as input to the
typechecking algorithm, and $T$ and $\Delta$ as outputs.


\begin{math}
\infer{\Gamma \vdash m\!:\!f(E) : T \dashv \Delta}
        {m\!:\!f : (S) \rightarrow T \in \Gamma & \Gamma \vdash E : S \dashv \Delta}
\\
\infer{\Gamma \vdash V : T \dashv \Delta}
        {V : T \in \Gamma}
\\
\infer{\Gamma \vdash \{ E_1, \dots , E_n\} : \{T_1, \dots, T_n\} \dashv \bigcup_{i = 1..n}\Delta_i}
        {\Gamma \vdash E_1 : T_1 \dashv \Delta_1 & \dots & \Gamma \vdash E_n : T_n \dashv \Delta_n}
\\
\infer{\Gamma \vdash E_1 , E_2 : T \dashv \Delta \cup \Upsilon}
        {\Gamma \vdash E_1 : S \dashv \Upsilon & \Gamma \cup \Upsilon \vdash E_2 : T \dashv \Delta}
\\
\infer{\Gamma \vdash V = E : T \dashv \Delta(V : T)}
        {\Gamma \vdash E : T \dashv \Delta}
\end{math}

## Subtyping

\begin{math}
\infer{T \sqsubseteq T}{}
\\
\infer{T_1 | \dots | T_n \sqsubseteq S}{\forall i . T_i \sqsubseteq S}
\\
\infer{T \sqsubseteq S_1 | \dots | S_n}{\exists i . T \sqsubseteq S_i}
\end{math}

# Gradual Type System

In gradual type systems there is particular type which means
"dynamically typed", and which is used to indicate which part of the
program should not be statically typed. In Erlang there is the type
$any()$ which already has this role, in the sense that any expression
has the type $any()$. So we will reuse $any()$ in our gradual type
system to mean that an expression is untyped.

## Compatibility relation

\begin{math}
any() \sim T
\\
T \sim any()
\\
R \sim T \; \& \; S \sim U \Rightarrow \{R,S\} \sim \{T,U\}
\end{math}

## Type Rules
