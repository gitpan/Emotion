use ExtUtils::MakeMaker;
WriteMakefile(
	      PREREQ_PM => {
		'XML::Parser' => 2.3,
		'List::Util' => 1,
	      },
	      NAME	   => 'Emotion',
	      VERSION_FROM => 'lib/Emotion.pm',
	      EXE_FILES    => ['pl/empathize'],
);
