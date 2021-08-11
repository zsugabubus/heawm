project('heawm', 'c',
  default_options: [
    'c_std=c11',
    'warning_level=2',
  ]
)

add_project_arguments('-D_XOPEN_SOURCE=700', language: 'c')

deps = [
  dependency('xcb-cursor'),
  dependency('xcb-keysyms'),
  dependency('xcb-randr'),
  dependency('xcb-shape'),
  dependency('xcb-xinput'),
  dependency('xcb-xfixes'),
  dependency('xcb-xkb'),
  dependency('xcb-xtest'),
  dependency('xcb-composite'),
  dependency('cairo'),
  dependency('xkbcommon-x11'),
]

if get_option('debug')
  deps += dependency('xcb-util')
  add_project_arguments('-DHEAWM_VERBOSE=3', language: 'c')
else
  add_project_arguments(['-DHEAWM_VERBOSE=0', '-DNDEBUG'], language: 'c')
endif

executable('heawm',
  'heawm.c',
  dependencies: deps,
  install: true,
)

vcs_tag(
  input: 'config.h.in',
  output: 'config.h',
  command: ['git', 'describe', '--always', '--tags', '--dirty', '--match', 'v*'],
)

custom_target(
  'man1',
  input: ['heawm.c', 'heawm.1.in'],
  output: 'heawm.1',
  command: ['manpage.gen', '@INPUT@', '@OUTPUT@'],
  install: true,
  install_dir: join_paths(get_option('mandir'), 'man1')
)
