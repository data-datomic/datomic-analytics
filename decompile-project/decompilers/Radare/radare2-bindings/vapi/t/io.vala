using Radare;

void main(string[] args) {
	var io = new Radare.RIO ();

	Radare.RIO.Desc? f = io.open ("dbg:///bin/ls", 0, 0);
	if (f==null) //(io.open ("dbg:///bin/ls", 0, 0))<1)
		critical("Cannot open file\n");
	
	uint8 buf[16];
	io.read_at (0x8048000, buf, 10);
	print ("0x8048000 : %02x %02x %02x %02x\n",
		buf[0], buf[1], buf[2], buf[3]);
/*
	Radare.List<IO.Plugin> handle = io.handlers;
	while (!handle->last()) {
		print(" Plugin: %s\n", handle->name);
		handle = handle->next();
	}
	handle->free();
*/
}
