;*=====================================================================*/
;*    serrano/prgm/project/skribe/src/bigloo/evapi.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 23 18:57:09 2003                          */
;*    Last change :  Sun Jul 11 11:32:23 2004 (serrano)                */
;*    Copyright   :  2003-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo eval declarations                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module skribe_evapi
   (import skribe_types
	   skribe_lib
	   skribe_api
	   skribe_engine
	   skribe_writer
	   skribe_output
	   skribe_eval
	   skribe_read
	   skribe_resolve
	   skribe_param
	   skribe_source
	   skribe_index
	   skribe_configure
	   skribe_lisp
	   skribe_xml
	   skribe_c
	   skribe_asm
	   skribe_bib
	   skribe_color
	   skribe_sui
	   skribe_debug)
   (eval   (export-all)))


