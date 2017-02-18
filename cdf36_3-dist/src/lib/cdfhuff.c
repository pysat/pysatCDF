/******************************************************************************
* Copyright 1996-2014 United States Government as represented by the
* Administrator of the National Aeronautics and Space Administration.
* All Rights Reserved.
******************************************************************************/
/******************************************************************************
*
*  NSSDC/CDF                               Huffman compression/decompression.
*
*  Version 1.0b, 2-Sep-97, Hughes STX.
*
*  Modification history:
*
*   V1.0  22-Jul-96, J Love     Original version.
*
*                               Algorithms in this file are from a book written
*                               by Mark Nelson, "The Data Compression Book."
*                               This book was published by M&T Books in 1992
*                               and no copyright  is claimed (see below).
*
*                               The Huffman code and Adaptive Huffman code
*                               are from pages 64-78 and 109-121, respectively.
*
*                               Slight modfications were made to the Huffman and 
*                               Adpative Huffman algorithms to work with the 
*                               existing CDF libray code.
*
*  Limits of Liability and Disclaimer of Warranty
*             The Author and Publisher of this book have used their best
*             efforts in preparing the book and the programs contained in it.
*             These efforts include the development, research, and testing
*             of the theories and programs to determine their effectiveness.
*
*             The Author and Publisher make no warranty of any kind,
*             expressed or implied, with regard to these programs or the
*             documentation contained in this book.  The Author and
*             Publisher shall not be liable in any event for incidental or
*             consequential damages in connection with, or arising out of,
*             the furnishing, performance, or use of these programs.
*
*  Liberal Code Use Policy (http://marknelson.us/code-use-policy)
*      It is my intention that anyone who buys the book or magazine be free
*      to use the source code in any form they please. I only request that
*      any use that involves public reproduction include proper attribution.
*      any use that involves public reproduction include proper attribution.
*
*      I assert that in no case will I initiate or cooperate with any attempt
*      to enforce the copyright on the source code, whether it belongs to me
*      or a publisher.
*
*      The code I publish is written for clarity, not efficiency. Once you
*      massage the code to work with your product, it is likely to be 
*      significantly different from the published code anyway.
*
*      Nobody from any of the companies that publish my work is even remotely
*      interested in pursuing people for unauthorized use of source code.
*      They don’t have anyone on their staffs to deal with problems like this,
*      and they probably don’t even want to think about it.
*
*      What the publishers are concerned about is you writing books or
*      articles that copy their stuff. Don’t do that, and you can be certain 
*      that they will be happy.
*
*      None of the code I publish is warranted in any way by me or the
*      publisher to be free of defects. If you need bullet-proof software
*      that is guaranteed to work as promised you will need to adopt a
*      methodology considerably more rigid than that of simply purchasing
*      a book or magazine.
*
*   V1.0a 28-Feb-97, J Love	Windows NT for MS Visual C/C++ on an IBM PC.
*   V1.0b  2-Sep-97, J Love	Type casting for ANSI C.
*
******************************************************************************/

#include "cdflib.h"

#if (!SUPPORT_HUFF || !SUPPORT_AHUFF) && defined(BORLANDC)
#pragma warn -par
#endif

/******************************************************************************
* Macros/prototypes.
******************************************************************************/

typedef uByte  BYTE;
typedef uInt16 WORD;
typedef uInt32 DWORD;

typedef uInt4 INT4;       /* Needed to pass a WORD by value. */

/*
 * The NODE structure is a node in the Huffman decoding tree.  It has a
 * count, which is its weight in the tree, and the node numbers of its
 * two children.  The saved_count member of the structure is only
 * there for debugging purposes, and can be safely taken out at any
 * time.  It just holds the intial count for each of the symbols, since
 * the count member is continually being modified as the tree grows.
 */
typedef struct tree_node {
    WORD count;
    WORD saved_count;
    int child_0;
    int child_1;
} NODE;

/*
 * A Huffman tree is set up for decoding, not encoding.  When encoding,
 * I first walk through the tree and build up a table of codes for
 * each symbol.  The codes are stored in this CODE structure.
 */
typedef struct code {
    WORD code;
    int code_bits;
} CODE;

/*
 * The special EOS symbol is 256, the first available symbol after all
 * of the possible bytes.  When decoding, reading this symbols
 * indicates that all of the data has been read in.
 */
#define END_OF_STREAM 256

typedef struct bit_file {
    vFILE *file;
    BYTE mask;
    int  rack;
} BIT_FILE;

#define END_OF_STREAM     256
#define ESCAPE            257
#define SYMBOL_COUNT      258
#define NODE_TABLE_COUNT  ((SYMBOL_COUNT*2)-1)
#define ROOT_NODE         0
#define MAX_WEIGHT        0x8000

/*
 * This data structure is all that is needed to maintain an adaptive
 * Huffman tree for both encoding and decoding.  The leaf array is a
 * set of indices into the nodes that indicate which node is the
 * parent of a symbol.  For example, to encode 'A', we would find the
 * leaf node by way of leaf[ 'A' ].  The next_free_node index is used
 * to tell which node is the next one in the array that can be used.
 * Since nodes are allocated when characters are read in for the first
 * time, this pointer keeps track of where we are in the node array.
 * Finally, the array of nodes is the actual Huffman tree.  The child
 * index is either an index pointing to a pair of children, or an
 * actual symbol value, depending on whether 'child_is_leaf' is true
 * or false.
 */

typedef struct tree {
    int leaf[ SYMBOL_COUNT ];
    int next_free_node;
    struct node {
	WORD weight;
	int parent;
	int child_is_leaf;
	int child;
    } nodes[ NODE_TABLE_COUNT ];
} TREE;

#if SUPPORT_HUFF || SUPPORT_AHUFF
static BIT_FILE *StartBitFile PROTOARGs((vFILE *fp));
static Logical EndOutputBitFile PROTOARGs((BIT_FILE *bit_file));
static Logical EndInputBitFile PROTOARGs((BIT_FILE *bit_file));
static Logical OutputBits PROTOARGs((
  BIT_FILE *bit_file, DWORD code, int count
));
static int InputBit PROTOARGs((BIT_FILE *bit_file));
#endif

#if SUPPORT_HUFF
static Logical output_counts PROTOARGs((BIT_FILE *output, NODE *nodes));
static Logical count_bytes PROTOARGs((
  vFILE *input, DWORD *counts, Int32 iSize
));
static void scale_counts PROTOARGs((DWORD *counts, NODE *nodes));
static int build_tree PROTOARGs((NODE *nodes));
static void convert_tree_to_code PROTOARGs((
  NODE *nodes, CODE *codes, INT4 code_so_far, int bits, int node
));
static CDFstatus compress_data PROTOARGs((
  vFILE *input, BIT_FILE *output, CODE *codes, Int32 iSize, CDFstatus iError,
  CDFstatus oError
));
static Logical input_counts PROTOARGs((BIT_FILE *input, NODE *nodes));
static CDFstatus expand_data PROTOARGs((
  BIT_FILE *input, vFILE *output, NODE *nodes, int root_node, CDFstatus iError,
  CDFstatus oError
));
#endif

#if SUPPORT_AHUFF
static void InitializeTree PROTOARGs((TREE *tree));
static Logical EncodeSymbol PROTOARGs((TREE *tree, INT4 c, BIT_FILE *output));
static int DecodeSymbol PROTOARGs((TREE *tree, BIT_FILE *input));
static void UpdateModel PROTOARGs((TREE *tree, int c));
static void RebuildTree PROTOARGs((TREE *tree));
static void swap_nodes PROTOARGs((TREE *tree, int i, int j));
static void add_new_node PROTOARGs((TREE *tree, int c));
static DWORD InputBits PROTOARGs((BIT_FILE *bit_file, int bit_count));
#endif

/******************************************************************************
* CompressHUFF0.
******************************************************************************/

STATICforIDL CDFstatus CompressHUFF0 (input, iOffset, iSize, iError,
				      oFp, oOffset, oSize, oError)
vFILE *input;
Int32 iOffset;
Int32 iSize;
CDFstatus iError;
vFILE *oFp;
Int32 oOffset;
Int32 *oSize;
CDFstatus oError;
{
#if SUPPORT_HUFF
  BIT_FILE *output; DWORD *counts; NODE *nodes; CODE *codes; 
  CDFstatus pStatus = CDF_OK; int root_node; long newOffset;
  if (!SEEKv(input,(long)iOffset,vSEEK_SET)) return iError;
  if (!SEEKv(oFp,(long)oOffset,vSEEK_SET)) return oError;
  output = StartBitFile (oFp);
  if (output == NULL) return BAD_MALLOC;
  *oSize = 0;
  counts = (DWORD *) CallocateMemory (256, sizeof(DWORD), NULL);
  if (counts == NULL) {
    cdf_FreeMemory (output, NULL);
    return BAD_MALLOC;
  }
  nodes = (NODE *) CallocateMemory (514, sizeof(NODE), NULL);
  if (nodes == NULL) {
    cdf_FreeMemory (counts, NULL);
    cdf_FreeMemory (output, NULL);
    return BAD_MALLOC;
  }
  codes = (CODE *) CallocateMemory (257, sizeof(CODE), NULL);
  if (codes == NULL) {
    cdf_FreeMemory (nodes, NULL);
    cdf_FreeMemory (counts, NULL);
    cdf_FreeMemory (output, NULL);
    return BAD_MALLOC;
  }
  if (!count_bytes(input,counts,iSize)) {
    cdf_FreeMemory (codes, NULL);
    cdf_FreeMemory (nodes, NULL);
    cdf_FreeMemory (counts, NULL);
    cdf_FreeMemory (output, NULL);
    return iError;
  }
  scale_counts (counts, nodes);
  if (!output_counts(output,nodes)) {
    cdf_FreeMemory (codes, NULL);
    cdf_FreeMemory (nodes, NULL);
    cdf_FreeMemory (counts, NULL);
    cdf_FreeMemory (output, NULL);
    return oError;
  }
  root_node = build_tree (nodes);
  convert_tree_to_code (nodes, codes, (INT4) 0, 0, root_node);
  if (!sX(compress_data(input,output,codes,iSize,iError,oError),&pStatus)) {
    cdf_FreeMemory (codes, NULL);
    cdf_FreeMemory (nodes, NULL);
    cdf_FreeMemory (counts, NULL);
    cdf_FreeMemory (output, NULL);
    return pStatus;
  }
  if (!EndOutputBitFile(output)) {
    cdf_FreeMemory (codes, NULL);
    cdf_FreeMemory (nodes, NULL);
    cdf_FreeMemory (counts, NULL);
    return oError;
  }
  newOffset = V_tell (oFp);
  if (newOffset == EOF) {
    cdf_FreeMemory (codes, NULL);
    cdf_FreeMemory (nodes, NULL);
    cdf_FreeMemory (counts, NULL);
    return oError;
  }
  *oSize = newOffset - oOffset;
  cdf_FreeMemory (codes, NULL);
  cdf_FreeMemory (nodes, NULL);
  cdf_FreeMemory (counts, NULL);
  return pStatus;
#else
  return UNKNOWN_COMPRESSION;
#endif
}

/******************************************************************************
* DecompressHUFF0.
******************************************************************************/

STATICforIDL CDFstatus DecompressHUFF0 (iFp, iOffset, iError,
					output, oOffset, oError)
vFILE *iFp;
Int32 iOffset;
CDFstatus iError;
vFILE *output;
Int32 oOffset;
CDFstatus oError;
{
#if SUPPORT_HUFF
  CDFstatus pStatus = CDF_OK; BIT_FILE *input; NODE *nodes; int root_node;
  if (!SEEKv(iFp,(long)iOffset,vSEEK_SET)) return iError;
  if (!SEEKv(output,(long)oOffset,vSEEK_SET)) return oError;
  input = StartBitFile (iFp);
  if (input == NULL) return BAD_MALLOC;
  nodes = (NODE *) CallocateMemory (514, sizeof(NODE), NULL);
  if (nodes == NULL) {
    cdf_FreeMemory (input, NULL);
    return BAD_MALLOC;
  }
  if (!input_counts(input,nodes)) {
    cdf_FreeMemory (nodes, NULL);
    cdf_FreeMemory (input, NULL);
    return iError;
  }
  root_node = build_tree( nodes );
  if (!sX(expand_data(input,output,nodes,root_node,iError,oError),&pStatus)) {
    cdf_FreeMemory (nodes, NULL);
    cdf_FreeMemory (input, NULL);
    return pStatus;
  }
  if (!EndInputBitFile(input)) {
    cdf_FreeMemory (nodes, NULL);
    return iError;
  }
  cdf_FreeMemory (nodes, NULL);
  return pStatus;
#else
  return UNKNOWN_COMPRESSION;
#endif
}

/******************************************************************************
* CompressAHUFF0.
******************************************************************************/

/*
 * The high level view of the compression routine is very simple.
 * First, we initialize the Huffman tree, with just the ESCAPE and
 * END_OF_STREAM symbols.  Then, we sit in a loop, encoding symbols,
 * and adding them to the model.  When there are no more characters
 * to send, the special END_OF_STREAM symbol is encoded.  The decoder
 * will later be able to use this symbol to know when to quit.
 */

STATICforIDL CDFstatus CompressAHUFF0 (input, iOffset, iSize, iError,
				       oFp, oOffset, oSize, oError)
vFILE *input;
Int32 iOffset;
Int32 iSize;
CDFstatus iError;
vFILE *oFp;
Int32 oOffset;
Int32 *oSize;
CDFstatus oError;
{
#if SUPPORT_AHUFF
  CDFstatus pStatus = CDF_OK; BIT_FILE *output; TREE *Tree;
  int c; Int32 i; long newOffset;
  if (!SEEKv(input,(long)iOffset,vSEEK_SET)) return iError;
  if (!SEEKv(oFp,(long)oOffset,vSEEK_SET)) return oError;
  output = StartBitFile (oFp);
  if (output == NULL) return BAD_MALLOC;
  *oSize = 0;
  Tree = (TREE *) CallocateMemory (1, sizeof(TREE), NULL);
  if (Tree == NULL) {
    cdf_FreeMemory (output, NULL);
    return BAD_MALLOC;
  }
  InitializeTree (Tree);
  for (i = 0; i < iSize; i++) {
     if ((c = V_getc(input)) == EOF) {
       cdf_FreeMemory (Tree, NULL);
       cdf_FreeMemory (output, NULL);
       return iError;
     }
     if (!EncodeSymbol(Tree,(INT4)c,output)) {
       cdf_FreeMemory (Tree, NULL);
       cdf_FreeMemory (output, NULL);
       return oError;
     }
     UpdateModel (Tree, c);
  }
  if (!EncodeSymbol(Tree,(INT4)END_OF_STREAM,output)) {
    cdf_FreeMemory (Tree, NULL);
    cdf_FreeMemory (output, NULL);
    return oError;
  }
  if (!EndOutputBitFile(output)) {
    cdf_FreeMemory (Tree, NULL);
    return oError;
  }
  newOffset = V_tell (oFp);
  if (newOffset == EOF) {
    cdf_FreeMemory (Tree, NULL);
    return oError;
  }
  *oSize = newOffset - oOffset;
  cdf_FreeMemory (Tree, NULL);
  return pStatus;
#else
  return UNKNOWN_COMPRESSION;
#endif
}

/******************************************************************************
* DecompressAHUFF0.
******************************************************************************/

STATICforIDL CDFstatus DecompressAHUFF0 (iFp, iOffset, iError,
					 output, oOffset, oError)
vFILE *iFp;
Int32 iOffset;
CDFstatus iError;
vFILE *output;
Int32 oOffset;
CDFstatus oError;
{
#if SUPPORT_AHUFF
  CDFstatus pStatus = CDF_OK; BIT_FILE *input; TREE *Tree; int c;
  if (!SEEKv(iFp,(long)iOffset,vSEEK_SET)) return iError;
  if (!SEEKv(output,(long)oOffset,vSEEK_SET)) return oError;
  input = StartBitFile (iFp);
  if (input == NULL) return BAD_MALLOC;
  Tree = (TREE *) CallocateMemory (1, sizeof(TREE), NULL);
  if (Tree == NULL) {
    cdf_FreeMemory (input, NULL);
    return BAD_MALLOC;
  }
  InitializeTree (Tree);
  while ((c = DecodeSymbol(Tree,input)) != END_OF_STREAM) {
    if (c == EOF) {
      cdf_FreeMemory (Tree, NULL);
      cdf_FreeMemory (input, NULL);
      return iError;
    }
    if (V_putc(c,output) == EOF) {
      cdf_FreeMemory (Tree, NULL);
      cdf_FreeMemory (input, NULL);
      return oError;
    }
    UpdateModel (Tree, c);
  }
  if (!EndInputBitFile(input)) {
    cdf_FreeMemory (Tree, NULL);
    return iError;
  }
  cdf_FreeMemory (Tree, NULL);
  return pStatus;
#else
  return UNKNOWN_COMPRESSION;
#endif
}

#if SUPPORT_HUFF
/******************************************************************************
* output_counts.
******************************************************************************/
/*
 * In order for the compressor to build the same model, I have to store
 * the symbol counts in the compressed file so the expander can read
 * them in.  In order to save space, I don't save all 256 symbols
 * unconditionally.  The format used to store counts looks like this:
 *
 *  start, stop, counts, start, stop, counts, ... 0
 *
 * This means that I store runs of counts, until all the non-zero
 * counts have been stored.  At this time the list is terminated by
 * storing a start value of 0.  Note that at least 1 run of counts has
 * to be stored, so even if the first start value is 0, I read it in.
 * It also means that even in an empty file that has no counts, I have
 * to pass at least one count.
 *
 * In order to efficiently use this format, I have to identify runs of
 * non-zero counts.  Because of the format used, I don't want to stop a
 * run because of just one or two zeros in the count stream.  So I have
 * to sit in a loop looking for strings of three or more zero values in
 * a row.
 *
 * This is simple in concept, but it ends up being one of the most
 * complicated routines in the whole program.  A routine that just
 * writes out 256 values without attempting to optimize would be much
 * simpler, but would hurt compression quite a bit on small files.
 *
 */
static Logical output_counts (output, nodes)
BIT_FILE *output;
NODE *nodes;
{
    int first;
    int last;
    int next;
    int i;

    first = 0;
    while ( first < 255 && nodes[ first ].count == 0 )
	    first++;
/*
 * Each time I hit the start of the loop, I assume that first is the
 * number for a run of non-zero values.  The rest of the loop is
 * concerned with finding the value for last, which is the end of the
 * run, and the value of next, which is the start of the next run.
 * At the end of the loop, I assign next to first, so it starts in on
 * the next run.
 */
    for ( ; first < 256 ; first = next ) {
	last = first + 1;
	for ( ; ; ) {
	    for ( ; last < 256 ; last++ )
		if ( nodes[ last ].count == 0 )
		    break;
	    last--;
	    for ( next = last + 1; next < 256 ; next++ )
		if ( nodes[ next ].count != 0 )
		    break;
	    if ( next > 255 )
		break;
	    if ( ( next - last ) > 3 )
		break;
	    last = next;
	};
/*
 * Here is where I output first, last, and all the counts in between.
 */
	if (V_putc(first,output->file) != first) return FALSE;
	if (V_putc(last,output->file) != last) return FALSE;
	for ( i = first ; i <= last ; i++ ) {
	    if (V_putc((int) nodes[ i ].count, output->file ) !=
		 (int) nodes[ i ].count ) return FALSE;
	}
    }
    if (V_putc(0,output->file) != 0) return FALSE;
    return TRUE;
}
#endif

#if SUPPORT_HUFF
/******************************************************************************
* count_bytes.
******************************************************************************/
/*
 * This routine counts the frequency of occurence of every byte in
 * the input file.  It marks the place in the input stream where it
 * started, counts up all the bytes, then returns to the place where
 * it started.  In most C implementations, the length of a file
 * cannot exceed an unsigned long, so this routine should always
 * work.
 */

static Logical count_bytes( input, counts, iSize )
vFILE *input;
DWORD *counts;
Int32 iSize;
{
    long input_marker;
    int c;
    Int32 i;

    input_marker = V_tell (input);
    if (input_marker == EOF) return FALSE;

    for (i = 0; i < iSize; i++) {
      if ((c = V_getc(input)) == EOF) return FALSE;
      counts[c]++;
    }

    if (!SEEKv(input,input_marker,vSEEK_SET)) return FALSE;
    return TRUE;
}
#endif

#if SUPPORT_HUFF
/******************************************************************************
* scale_counts.
******************************************************************************/
/*
 * In order to limit the size of my Huffman codes to 16 bits, I scale
 * my counts down so they fit in an unsigned char, and then store them
 * all as initial weights in my NODE array.  The only thing to be
 * careful of is to make sure that a node with a non-zero count doesn't
 * get scaled down to 0.  Nodes with values of 0 don't get codes.
 */
static void scale_counts( counts, nodes )
DWORD *counts;
NODE *nodes;
{
    DWORD max_count;
    int i;

    max_count = 0;
    for ( i = 0 ; i < 256 ; i++ )
       if ( counts[ i ] > max_count )
	   max_count = counts[ i ];
    if ( max_count == 0 ) {
	counts[ 0 ] = 1;
	max_count = 1;
    }
    max_count = max_count / 255;
    max_count = max_count + 1;
    for ( i = 0 ; i < 256 ; i++ ) {
    nodes[ i ].count = (WORD) ( counts[ i ] / max_count );
	if ( nodes[ i ].count == 0 && counts[ i ] != 0 )
	    nodes[ i ].count = 1;
    }
    nodes[ END_OF_STREAM ].count = 1;
}
#endif

#if SUPPORT_HUFF
/******************************************************************************
* build_tree.
******************************************************************************/
/*
 * Building the Huffman tree is fairly simple.  All of the active nodes
 * are scanned in order to locate the two nodes with the minimum
 * weights.  These two weights are added together and assigned to a new
 * node.  The new node makes the two minimum nodes into its 0 child
 * and 1 child.  The two minimum nodes are then marked as inactive.
 * This process repeats until their is only one node left, which is the
 * root node.  The tree is done, and the root node is passed back
 * to the calling routine.
 *
 * Node 513 is used here to arbitratily provide a node with a guaranteed
 * maximum value.  It starts off being min_1 and min_2.  After all active
 * nodes have been scanned, I can tell if there is only one active node
 * left by checking to see if min_1 is still 513.
 */
static int build_tree( nodes )
NODE *nodes;
{
    int next_free;
    int i;
    int min_1;
    int min_2;

    nodes[ 513 ].count = 0xffff;
    for ( next_free = END_OF_STREAM + 1 ; ; next_free++ ) {
	min_1 = 513;
	min_2 = 513;
	for ( i = 0 ; i < next_free ; i++ )
	    if ( nodes[ i ].count != 0 ) {
		if ( nodes[ i ].count < nodes[ min_1 ].count ) {
		    min_2 = min_1;
		    min_1 = i;
		} else if ( nodes[ i ].count < nodes[ min_2 ].count )
		    min_2 = i;
	    }
	if ( min_2 == 513 )
	    break;
	nodes[ next_free ].count = nodes[ min_1 ].count
				   + nodes[ min_2 ].count;
	nodes[ min_1 ].saved_count = nodes[ min_1 ].count;
	nodes[ min_1 ].count = 0;
	nodes[ min_2 ].saved_count =  nodes[ min_2 ].count;
	nodes[ min_2 ].count = 0;
	nodes[ next_free ].child_0 = min_1;
	nodes[ next_free ].child_1 = min_2;
    }
    next_free--;
    nodes[ next_free ].saved_count = nodes[ next_free ].count;
    return( next_free );
}
#endif

#if SUPPORT_HUFF
/******************************************************************************
* convert_tree_to_code.
******************************************************************************/
/*
 * Since the Huffman tree is built as a decoding tree, there is
 * no simple way to get the encoding values for each symbol out of
 * it.  This routine recursively walks through the tree, adding the
 * child bits to each code until it gets to a leaf.  When it gets
 * to a leaf, it stores the code value in the CODE element, and
 * returns.
 */
static void convert_tree_to_code( nodes, codes, code_so_far, bits, node )
NODE *nodes;
CODE *codes;
INT4 code_so_far;
int bits;
int node;
{
  if (node <= END_OF_STREAM) {
    codes[node].code = (WORD) code_so_far;
    codes[node].code_bits = bits;
    return;
  }
  code_so_far <<= 1;
  bits++;
  convert_tree_to_code (nodes, codes, code_so_far, bits, nodes[node].child_0 );
  convert_tree_to_code (nodes, codes, (INT4) (code_so_far | 1),
			bits, nodes[node].child_1 );
  return;
}
#endif

#if SUPPORT_HUFF
/******************************************************************************
* compress_data.
******************************************************************************/
/*
 * Once the tree gets built, and the CODE table is built, compressing
 * the data is a breeze.  Each byte is read in, and its corresponding
 * Huffman code is sent out.
 */

static CDFstatus compress_data( input, output, codes, iSize, iError, oError)
vFILE *input;
BIT_FILE *output;
CODE *codes;
Int32 iSize;
CDFstatus iError;
CDFstatus oError;
{
    int c;
    long i;

    for (i = 0; i < iSize; i++) {
       if ((c = V_getc(input)) == EOF) return iError;
       if (!OutputBits(output,(DWORD)codes[c].code,codes[ c ].code_bits)) {
	 return oError;
       }
    }
    if (!OutputBits(output,(DWORD)codes[END_OF_STREAM].code,
		    codes[END_OF_STREAM].code_bits)) return oError;
    return CDF_OK;
}
#endif

#if SUPPORT_HUFF || SUPPORT_AHUFF
/******************************************************************************
* StartBitFile.
******************************************************************************/

static BIT_FILE *StartBitFile (fp)
vFILE *fp;
{
  BIT_FILE *bit_file;
  bit_file = (BIT_FILE *) CallocateMemory (1, sizeof(BIT_FILE), NULL);
  if (bit_file == NULL) return NULL;
  bit_file->file = fp;
  bit_file->rack = 0;
  bit_file->mask = 0x80;
  return bit_file;
}
#endif

#if SUPPORT_HUFF || SUPPORT_AHUFF
/******************************************************************************
* EndOutputBitFile.
******************************************************************************/

static Logical EndOutputBitFile (bit_file)
BIT_FILE *bit_file;
{
  Logical status = TRUE;
  if (bit_file->mask != 0x80) {
    if (V_putc(bit_file->rack,
	       bit_file->file) != bit_file->rack) status = FALSE;
  }
  cdf_FreeMemory (bit_file, NULL);
  return status;
}
#endif

#if SUPPORT_HUFF || SUPPORT_AHUFF
/******************************************************************************
* EndInputBitFile.
******************************************************************************/

static Logical EndInputBitFile (bit_file)
BIT_FILE *bit_file;
{
  cdf_FreeMemory (bit_file, NULL);
  return TRUE;
}
#endif

#if SUPPORT_HUFF || SUPPORT_AHUFF
/******************************************************************************
* OutputBits.
******************************************************************************/

static Logical OutputBits( bit_file, code, count )
BIT_FILE *bit_file;
DWORD code;
int count;
{
    DWORD mask;

    mask = 1L << ( count - 1 );
    while ( mask != 0) {
	if ( mask & code ) bit_file->rack |= bit_file->mask;
	bit_file->mask >>= 1;
	if ( bit_file->mask == 0 ) {
	    if (V_putc( bit_file->rack, bit_file->file ) != bit_file->rack ) {
		return FALSE;
	    }
	    bit_file->rack = 0;
	    bit_file->mask = 0x80;
	}
	mask >>= 1;
    }
    return TRUE;
}
#endif

#if SUPPORT_HUFF || SUPPORT_AHUFF
/******************************************************************************
* InputBit.
******************************************************************************/

static int InputBit (bit_file)
BIT_FILE *bit_file;
{
    int value;
    if ( bit_file->mask == 0x80 ) {
	bit_file->rack = V_getc( bit_file->file );
	if ( bit_file->rack == EOF ) return EOF;
    }
    value = bit_file->rack & bit_file->mask;
    bit_file->mask >>= 1;
    if ( bit_file->mask == 0 ) bit_file->mask = 0x80;
    return ( value ? 1 : 0 );
}
#endif

#if SUPPORT_AHUFF
/******************************************************************************
* InputBits.
******************************************************************************/

static DWORD InputBits( bit_file, bit_count )
 BIT_FILE *bit_file;
 int bit_count;
{
    DWORD mask;
    DWORD return_value;

    mask = 1L << ( bit_count - 1 );
    return_value = 0;
    while ( mask != 0) {
	if ( bit_file->mask == 0x80 ) {
	    bit_file->rack = V_getc( bit_file->file );
	    if ( bit_file->rack == EOF ) return ((DWORD) EOF);
	}
	if ( bit_file->rack & bit_file->mask ) return_value |= mask;
	mask >>= 1;
	bit_file->mask >>= 1;
	if ( bit_file->mask == 0 ) bit_file->mask = 0x80;
    }
    return( return_value );
}
#endif

#if SUPPORT_HUFF
/******************************************************************************
* input_counts.
******************************************************************************/
/*
 * When expanding, I have to read in the same set of counts.  This is
 * quite a bit easier that the process of writing them out, since no
 * decision making needs to be done.  All I do is read in first, check
 * to see if I am all done, and if not, read in last and a string of
 * counts.
 */

static Logical input_counts( input, nodes )
BIT_FILE *input;
NODE *nodes;
{
    int first;
    int last;
    int i;
    int c;

    for ( i = 0 ; i < 256 ; i++ ) nodes[ i ].count = 0;
    if ( ( first = V_getc( input->file ) ) == EOF ) return FALSE;
    if ( ( last = V_getc( input->file ) ) == EOF ) return FALSE;
    for ( ; ; ) {
	for ( i = first ; i <= last ; i++ ) {
	    if ( ( c = V_getc( input->file ) ) == EOF ) return FALSE;
	    nodes[ i ].count = (WORD) c;
	}
	if ( ( first = V_getc( input->file ) ) == EOF ) return FALSE;
	if ( first == 0 ) break;
	if ( ( last = V_getc( input->file ) ) == EOF ) return FALSE;
    }
    nodes[ END_OF_STREAM ].count = 1;
    return TRUE;
}
#endif

#if SUPPORT_HUFF
/******************************************************************************
* expand_data.
******************************************************************************/
/*
 * Expanding compressed data is a little harder than the compression
 * phase.  As each new symbol is decoded, the tree is traversed,
 * starting at the root node, reading a bit in, and taking either the
 * child_0 or child_1 path.  Eventually, the tree winds down to a
 * leaf node, and the corresponding symbol is output.  If the symbol
 * is the END_OF_STREAM symbol, it doesn't get written out, and
 * instead the whole process terminates.
 */
static CDFstatus expand_data( input, output, nodes, root_node, iError, oError)
BIT_FILE *input;
vFILE *output;
NODE *nodes;
int root_node;
CDFstatus iError;
CDFstatus oError;
{
    int node;

    for ( ; ; ) {
	node = root_node;
	do {
	    int bit = InputBit (input);
	    if (bit == EOF) return iError;
	    if (bit)
		node = nodes[ node ].child_1;
	    else
		node = nodes[ node ].child_0;
	} while ( node > END_OF_STREAM );
	if ( node == END_OF_STREAM ) break;
	if ( ( V_putc( node, output ) ) != node ) return oError;
    }
    return CDF_OK;
}
#endif

#if SUPPORT_AHUFF
/******************************************************************************
* InitializeTree.
******************************************************************************/

/*
 * When performing adaptive compression, the Huffman tree starts out
 * very nearly empty.  The only two symbols present initially are the
 * ESCAPE symbol and the END_OF_STREAM symbol.  The ESCAPE symbol has to
 * be included so we can tell the expansion prog that we are transmitting a
 * previously unseen symbol.  The END_OF_STREAM symbol is here because
 * it is greater than eight bits, and our ESCAPE sequence only allows for
 * eight bit symbols following the ESCAPE code.
 *
 * In addition to setting up the root node and its two children, this
 * routine also initializes the leaf array.  The ESCAPE and END_OF_STREAM
 * leaf elements are the only ones initially defined, the rest of the leaf
 * elements are set to -1 to show that they aren't present in the
 * Huffman tree yet.
 */

static void InitializeTree( tree )
TREE *tree;
{
    int i;

    tree->nodes[ ROOT_NODE ].child             = ROOT_NODE + 1;
    tree->nodes[ ROOT_NODE ].child_is_leaf     = FALSE;
    tree->nodes[ ROOT_NODE ].weight            = 2;
    tree->nodes[ ROOT_NODE ].parent            = -1;

    tree->nodes[ ROOT_NODE + 1 ].child         = END_OF_STREAM;
    tree->nodes[ ROOT_NODE + 1 ].child_is_leaf = TRUE;
    tree->nodes[ ROOT_NODE + 1 ].weight        = 1;
    tree->nodes[ ROOT_NODE + 1 ].parent        = ROOT_NODE;
    tree->leaf[ END_OF_STREAM ]                = ROOT_NODE + 1;

    tree->nodes[ ROOT_NODE + 2 ].child         = ESCAPE;
    tree->nodes[ ROOT_NODE + 2 ].child_is_leaf = TRUE;
    tree->nodes[ ROOT_NODE + 2 ].weight        = 1;
    tree->nodes[ ROOT_NODE + 2 ].parent        = ROOT_NODE;
    tree->leaf[ ESCAPE ]                       = ROOT_NODE + 2;

    tree->next_free_node                       = ROOT_NODE + 3;

    for ( i = 0 ; i < END_OF_STREAM ; i++ ) tree->leaf[ i ] = -1;
}
#endif

#if SUPPORT_AHUFF
/******************************************************************************
* EncodeSymbol.
******************************************************************************/

/*
 * This routine is responsible for taking a symbol, and converting
 * it into the sequence of bits dictated by the Huffman tree.  The
 * only complication is that we are working are way up from the leaf
 * to the root, and hence are getting the bits in reverse order.  This
 * means we have to rack up the bits in an integer and then send them
 * out after they are all accumulated.  In this version of the program,
 * we keep our codes in a long integer, so the maximum count is set
 * to an arbitray limit of 0x8000.  It could be set as high as 65535
 * if desired.
 */

static Logical EncodeSymbol( tree, c, output )
TREE *tree;
INT4 c;
BIT_FILE *output;
{
    DWORD code;
    DWORD current_bit;
    int code_size;
    int current_node;

    code = 0;
    current_bit = 1;
    code_size = 0;
    current_node = tree->leaf[(int)c];
    if ( current_node == -1 ) current_node = tree->leaf[ ESCAPE ];
    while ( current_node != ROOT_NODE ) {
	if ( ( current_node & 1 ) == 0 ) code |= current_bit;
	current_bit <<= 1;
	code_size++;
	current_node = tree->nodes[ current_node ].parent;
    };
    if (!OutputBits(output,code,code_size)) return FALSE;
    if ( tree->leaf[(int)c] == -1 ) {
	if (!OutputBits(output,(DWORD)c,8)) return FALSE;
	add_new_node( tree, (int) c );
    }
    return TRUE;
}
#endif

#if SUPPORT_AHUFF
/******************************************************************************
* DecodeSymbol.
******************************************************************************/

/*
 * Decoding symbols is easy.  We start at the root node, then go down
 * the tree until we reach a leaf.  At each node, we decide which
 * child to take based on the next input bit.  After getting to the
 * leaf, we check to see if we read in the ESCAPE code.  If we did,
 * it means that the next symbol is going to come through in the next
 * eight bits, unencoded.  If that is the case, we read it in here,
 * and add the new symbol to the table.
 */

static int DecodeSymbol (tree, input)
TREE *tree;
BIT_FILE *input;
{
    int current_node;
    int c;
    int bit;

    current_node = ROOT_NODE;
    while ( !tree->nodes[ current_node ].child_is_leaf ) {
	current_node = tree->nodes[ current_node ].child;
	bit = InputBit (input);
	if (bit == EOF) return EOF;
	current_node += bit;
    }
    c = tree->nodes[ current_node ].child;
    if ( c == ESCAPE ) {
	c = (int) InputBits( input, 8 );
	if (c == EOF) return EOF;
	add_new_node( tree, c );
    }
    return( c );
}
#endif

#if SUPPORT_AHUFF
/******************************************************************************
* UpdateModel.
******************************************************************************/

/*
 * UpdateModel is called to increment the count for a given symbol.
 * After incrementing the symbol, this code has to work its way up
 * through the parent nodes, incrementing each one of them.  That is
 * the easy part.  The hard part is that after incrementing each
 * parent node, we have to check to see if it is now out of the proper
 * order.  If it is, it has to be moved up the tree into its proper
 * place.
 */

static void UpdateModel( tree, c )
TREE *tree;
int c;
{
    int current_node;
    int new_node;

    if ( tree->nodes[ ROOT_NODE].weight == MAX_WEIGHT ) RebuildTree( tree );
    current_node = tree->leaf[ c ];
    while ( current_node != -1 ) {
	tree->nodes[ current_node ].weight++;
	for ( new_node = current_node ; new_node > ROOT_NODE ; new_node-- )
	    if ( tree->nodes[ new_node - 1 ].weight >=
		 tree->nodes[ current_node ].weight )
		break;
	if ( current_node != new_node ) {
	    swap_nodes( tree, current_node, new_node );
	    current_node = new_node;
	}
	current_node = tree->nodes[ current_node ].parent;
    }

    return;
}
#endif

#if SUPPORT_AHUFF
/******************************************************************************
* RebuildTree.
******************************************************************************/

/*
 * Rebuilding the tree takes place when the counts have gone too
 * high.  From a simple point of view, rebuilding the tree just means that
 * we divide every count by two.  Unfortunately, due to truncation effects,
 * this means that the tree's shape might change.  Some nodes might move
 * up due to cumulative increases, while others may move down.
 */

static void RebuildTree( tree )
TREE *tree;
{
    int i;
    int j;
    int k;
    WORD weight;

/*
 * To start rebuilding the table,  I collect all the leaves of the Huffman
 * tree and put them in the end of the tree.  While I am doing that, I
 * scale the counts down by a factor of 2.
 */
    j = tree->next_free_node - 1;
    for ( i = j ; i >= ROOT_NODE ; i-- ) {
	if ( tree->nodes[ i ].child_is_leaf ) {
	    tree->nodes[ j ] = tree->nodes[ i ];
	    tree->nodes[ j ].weight = (tree->nodes[j].weight + 1) / ((WORD) 2);
	    j--;
	}
    }

/*
 * At this point, j points to the first free node.  I now have all the
 * leaves defined, and need to start building the higher nodes on the
 * tree. I will start adding the new internal nodes at j.  Every time
 * I add a new internal node to the top of the tree, I have to check to
 * see where it really belongs in the tree.  It might stay at the top,
 * but there is a good chance I might have to move it back down.  If it
 * does have to go down, I use the memmove() function to scoot everyone
 * bigger up by one node.  Note that memmove() may have to be change
 * to memcpy() on some UNIX systems.  The parameters are unchanged, as
 * memmove and  memcpy have the same set of parameters.
 */
    for ( i = tree->next_free_node - 2 ; j >= ROOT_NODE ; i -= 2, j-- ) {
	k = i + 1;
	tree->nodes[ j ].weight = tree->nodes[ i ].weight +
				  tree->nodes[ k ].weight;
	weight = tree->nodes[ j ].weight;
	tree->nodes[ j ].child_is_leaf = FALSE;
	for ( k = j + 1 ; weight < tree->nodes[ k ].weight ; k++ )
	    ;
	k--;
	memmove( &tree->nodes[ j ], &tree->nodes[ j + 1 ],
		 ( k - j ) * sizeof( struct node ) );
	tree->nodes[ k ].weight = weight;
	tree->nodes[ k ].child = i;
	tree->nodes[ k ].child_is_leaf = FALSE;
    }
/*
 * The final step in tree reconstruction is to go through and set up
 * all of the leaf and parent members.  This can be safely done now
 * that every node is in its final position in the tree.
 */
    for ( i = tree->next_free_node - 1 ; i >= ROOT_NODE ; i-- ) {
	if ( tree->nodes[ i ].child_is_leaf ) {
	    k = tree->nodes[ i ].child;
	    tree->leaf[ k ] = i;
	} else {
	    k = tree->nodes[ i ].child;
	    tree->nodes[ k ].parent = tree->nodes[ k + 1 ].parent = i;
	}
    }

    return;
}
#endif

#if SUPPORT_AHUFF
/******************************************************************************
* swap_nodes.
******************************************************************************/

/*
 * Swapping nodes takes place when a node has grown too big for its
 * spot in the tree.  When swapping nodes i and j, we rearrange the
 * tree by exchanging the children under i with the children under j.
 */

static void swap_nodes( tree, i, j )
TREE *tree;
int i;
int j;
{
    struct node temp;

    if ( tree->nodes[ i ].child_is_leaf )
	tree->leaf[ tree->nodes[ i ].child ] = j;
    else {
	tree->nodes[ tree->nodes[ i ].child ].parent = j;
	tree->nodes[ tree->nodes[ i ].child + 1 ].parent = j;
    }
    if ( tree->nodes[ j ].child_is_leaf )
	tree->leaf[ tree->nodes[ j ].child ] = i;
    else {
	tree->nodes[ tree->nodes[ j ].child ].parent = i;
	tree->nodes[ tree->nodes[ j ].child + 1 ].parent = i;
    }
    temp = tree->nodes[ i ];
    tree->nodes[ i ] = tree->nodes[ j ];
    tree->nodes[ i ].parent = temp.parent;
    temp.parent = tree->nodes[ j ].parent;
    tree->nodes[ j ] = temp;

    return;
}
#endif

#if SUPPORT_AHUFF
/******************************************************************************
* add_new_node.
******************************************************************************/

/*
 * Adding a new node to the tree is pretty simple.  It is just a matter
 * of splitting the lightest-weight node in the tree, which is the highest
 * valued node.  We split it off into two new nodes, one of which is the
 * one being added to the tree.  We assign the new node a weight of 0,
 * so the tree doesn't have to be adjusted.  It will be updated later when
 * the normal update process occurs.  Note that this code assumes that
 * the lightest node has a leaf as a child.  If this is not the case,
 * the tree would be broken.
 */

static void add_new_node( tree, c )
TREE *tree;
int c;
{
    int lightest_node;
    int new_node;
    int zero_weight_node;

    lightest_node = tree->next_free_node - 1;
    new_node = tree->next_free_node;
    zero_weight_node = tree->next_free_node + 1;
    tree->next_free_node += 2;

    tree->nodes[ new_node ] = tree->nodes[ lightest_node ];
    tree->nodes[ new_node ].parent = lightest_node;
    tree->leaf[ tree->nodes[ new_node ].child ] = new_node;

    tree->nodes[ lightest_node ].child         = new_node;
    tree->nodes[ lightest_node ].child_is_leaf = FALSE;

    tree->nodes[ zero_weight_node ].child           = c;
    tree->nodes[ zero_weight_node ].child_is_leaf   = TRUE;
    tree->nodes[ zero_weight_node ].weight          = 0;
    tree->nodes[ zero_weight_node ].parent          = lightest_node;
    tree->leaf[ c ] = zero_weight_node;
    return;
}
#endif
