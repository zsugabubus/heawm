project('heawm', 'c',
	default_options: [
		'c_std=c11',
		'warning_level=3',
	]
)

deps = [
	dependency('cairo'),
	dependency('xcb-cursor'),
	dependency('xcb-keysyms'),
	dependency('xcb-randr'),
	dependency('xcb-shape'),
	dependency('xcb-xinput'),
	dependency('xcb-xkb'),
	dependency('xcb-xtest'),
	dependency('xcb-xrm'),
	dependency('xkbcommon-x11'),
]

if get_option('debug')
	deps += dependency('xcb-util')
endif

executable('heawm',
	'heawm.c',
	'win_hash.c',
	dependencies: deps,
	install: true,
)

pod2man = find_program('pod2man')

custom_target(
	'man',
	output: meson.project_name() + '.1',
	input: 'README.pod',
	command: [
		pod2man,
		'--section=1',
		'--center=X11 window manager',
		'--name=' + meson.project_name(),
		'@INPUT@', '@OUTPUT@'
	],
	install: true,
	install_dir: get_option('mandir') / 'man1'
)
