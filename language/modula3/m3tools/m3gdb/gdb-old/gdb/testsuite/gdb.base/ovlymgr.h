/*
 * Sample runtime overlay manager.
 */

#ifdef NO_PROTOTYPES
#define PARAMS(paramlist) ()
#else
#define PARAMS(paramlist) paramlist
#endif

/* Entry Points: */

void OverlayLoad PARAMS((int ovlyno));
void OverlayUnload PARAMS((int ovlyno));

