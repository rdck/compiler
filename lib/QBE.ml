(******************************************************************************)
(* QBE IR                                                                     *)
(******************************************************************************)

open Core

type identifier = string

type base_type =
  | Word      (* w *)
  | Long      (* l *)
  | Single    (* s *)
  | Double    (* d *)

type extended_type =
  | BaseType of base_type
  | Byte
  | HalfWord

type constant =
  | ConstantInteger of int
  | ConstantSingle of float
  | ConstantDouble of float
  | ConstantSymbol of identifier

type dynamic_constant =
  | Constant of constant
  | ThreadLocal of identifier

type value =
  | DynamicConstant of dynamic_constant
  | Value of identifier

type linkage =
  | LinkageExport
  | LinkageThread
  | LinkageSection of string * string

type subtype =
  | STType of extended_type
  | STSymbol of identifier

type subtypes = subtype * int

type type_def =
  | TypeDef of identifier * int * subtypes
  | TypeUnion of identifier * int * subtypes list
  | TypeOpaque of identifier * int * int

type data_item =
  | DISymbol of identifier * int
  | DIString of string
  | DIConstant of constant

type operation =
  | Add
  | Sub

type instruction = {
  target : identifier ;
  size : base_type ;
  operation : operation ;
  left : value ;
  right : value ;
}
