from test_support import exec_cmd, build_and_run

exec_cmd('ada2wsdl',
         ['-q', '-Parray_chars', '-lit',
          '-o', 'character.wsdl', 'character_services.ads'])

exec_cmd('wsdl2aws',
         ['-q', '-f', '-types', 'character_types', '-spec',
          'character_services', '-cb', 'character.wsdl'])

build_and_run('array_chars')
