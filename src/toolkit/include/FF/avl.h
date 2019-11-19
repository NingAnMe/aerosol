/* AVL.H */

typedef struct	_leaf
{
	struct  _leaf	*left     ;
	struct  _leaf	*right	  ;
	unsigned	size : 14 ;
	unsigned	bal  :  2 ;
}
HEADER;
			/* Possible values of bal field. Can be	*/
			/* any three consecutive numbers but	*/
			/* L < B < R must hold.			*/
#define L	 0	/* 	Left  subtree is larger		*/
#define	B	 1	/* 	Balanced subtree		*/
#define R	 2	/* 	Right subtree is larger		*/


/*int	delete  ( HEADER**, HEADER*,  int(*)()	);
HEADER *insert  ( HEADER**, HEADER*,  int(*)()	);
HEADER *find    ( HEADER* , HEADER*,  int(*)()	);
void	tprint  ( HEADER* , int(*)(), FILE*	);
HEADER	*talloc ( int 	  			);
void	tfree   ( HEADER* 			);
*/

