project('sqlite', 'c',
    version : '3.31.1',
    license : 'pd')

thread_dep = dependency('threads')

cc = meson.get_compiler('c')
dl_dep = cc.find_library('dl', required: false)

install_headers('sqlite3.h', 'sqlite3ext.h')

sqinc = include_directories('.')
sqlite = library('sqlite3', 'sqlite3.c',
    c_args : ['-DSQLITE_ENABLE_JSON1'],
    include_directories : sqinc,
    dependencies : [thread_dep, dl_dep],
    install : true)
sqlite_dep = declare_dependency(link_with : sqlite,
    include_directories : sqinc)

pkg = import('pkgconfig')
pkg.generate(filebase : 'sqlite3',
    name : 'SQLite',
    version : meson.project_version(),
    description : 'SQL database engine',
    libraries : [sqlite])
