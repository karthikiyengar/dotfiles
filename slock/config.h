/* user and group to drop privileges to */
static const char *user  = "nobody";
static const char *group = "nogroup";

static const char *colorname[NUMCOLS] = {
	[INIT] =   "#6E9896",   /* after initialization */
	[INPUT] =  "#FCE8BA",   /* during input */
	[FAILED] = "#FC8064",   /* wrong password */
};

/* treat a cleared input like a wrong password (color) */
static const int failonclear = 1;
