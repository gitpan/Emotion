use ExtUtils::MakeMaker;
WriteMakefile(
	      PREREQ_PM => {
		'XML::Parser' => 2.3,
		'XML::Writer' => 0.4,
	      },
	      NAME	   => 'Emotion',
	      VERSION_FROM => 'lib/Emotion.pm',
	      EXE_FILES    => ['pl/prepare_index',
			       'pl/prepare_transcript'],
);
