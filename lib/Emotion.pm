# Copyright (C) 2000 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

use strict;
package Emotion;
our $VERSION = '0.11';

our $Stem;
our $DialogID;
our %Link;  # resolve symbolic links
our %Open;  # by label

sub set_transcript {
    my ($xml) = @_;
    my $stem = $xml;
    $stem =~ s/^.*\///;
    die "$stem is XML?"
	if $stem !~ s/\.xml$//;
    $Stem = $stem;
    %Link = ();
    %Open = ();
    $DialogID=0;
}

our $Speaker;
our $PhraseID;

sub set_speaker {
    my ($who) = @_;
    $Speaker = $who;
    $PhraseID=0;
    ++$DialogID;
}

sub unresolved { values %Open }

our %TypeCode = (destroys => 0,
		 steals => 1,
		 uneasy => 2,
		 exposes => 3,
		 impasse => 4,
		 admires => 5,
		 observes => 6,
		 accepts => 7,
		 ready => 8);

package Emotion::Atom;
use List::Util qw(sum);

sub new {
    my ($class, $expat, $attr) = @_;
    my $o = bless $attr, $class;

    $o->{stem} = $Stem;
    $o->{dialog_id} = $DialogID;
    $o->{phrase_id} = ++$PhraseID;

    my $type = $o->{type};

    my $id = $o->{id};
    if ($id) {
	$expat->xpcroak("Link `$id' doesn't match \\w+")
	    if ($id !~ m/^\w+$/);

	if (exists $Link{$id}) {
	    my $try = $id;
	    ++$try while exists $Link{$try};
	    $expat->xpcroak("Link collision with `$id' at talk $Link{$id}{dialog_id}, try `$try'");
	}
	$Link{$id} = $o;
    }

    $expat->xpcroak("reply renamed to re")
	if exists $o->{reply};
    $expat->xpcroak("answer renamed to re")
	if exists $o->{answer};

    for my $kind (qw(re echo revoke amend context)) {
	next if !exists $o->{$kind};
	my $to = $o->{$kind};
	$o->{$kind} = $Link{$to};
	$expat->xpcroak("broken $kind link `$to'")
	    if !$o->{$kind};
    }

    # tension & intensity
    if ($type =~ m/^(steals|exposes|admires|accepts)$/) { #spin
	my $before = $o->{before};
	my $after = $o->{after};
	my $tension = $o->{tension};
	if (exists $o->{intensity}) {
	    $expat->xpcarp("rename intensity to tension");
	    $tension = $o->{tension} = delete $o->{intensity};
	}
	$expat->xpcroak("before or after or intensity?")
	    if (defined($before) + defined($after) + defined($tension) != 1);

	my $i = $before || $after || $tension;
	$expat->xpcroak("$i?")
		if $i !~ m/^(focused|relaxed|stifled)$/;
    } elsif ($type eq 'impasse') {
	my $tension = $o->{tension};
	if (exists $o->{intensity}) {
	    $expat->xpcarp("rename intensity to tension");
	    $tension = $o->{tension} = delete $o->{intensity};
	}
	$expat->xpcroak("tension=focused|relaxed|stifled?")
	    if !$tension;
    } elsif ($type =~ m/^(uneasy|observes|ready)$/) {
	my $i = $o->{intensity};
	$expat->xpcroak("intensity=$i? (gentle|forceful|extreme)")
	    if !$i || $i !~ m/^(gentle|forceful|extreme)$/;
    } else { #destroys
	$expat->xpcroak("no intensity needed")
	    if exists $o->{intensity};
	$expat->xpcroak("no tension needed")
	    if exists $o->{tension};
    }
	
    if (exists $o->{initiative}) {
	$expat->xpcarp("initiative renamed to initiator");
	$o->{initiator} = delete $o->{initiative};
    }

    if ($type =~ m/^(impasse|destroys)$/) {
	$expat->xpcroak("$type initiator is always left")
	    if exists $o->{initiator};
	$o->{initiator} = 'left';
    } elsif (exists $o->{initiator}) {
	my $i = $o->{initiator};
	if (exists $o->{left} xor exists $o->{right}) {
	    $expat->xpcroak("initiator is who?")
		if $i =~ m/^(left|right)$/;
	    if (exists $o->{left}) {
		$o->{right} = $i;
		$o->{initiator} = 'right'
	    } else {
		$o->{left} = $i;
		$o->{initiator} = 'left';
	    }
	} else {
	    $expat->xpcroak("initiator `$i' is left or right?")
		if $i !~ m/^(left|right)$/;
	}
    }

    if ($Speaker) {
	for my $role (qw(left right)) {
	    next if exists $o->{$role};
	    $o->{$role} = $Speaker;
	    $o->{initiator} ||= $role
		if $type !~ m/^(ready|observes|uneasy|impasse|destroys)$/;
	}
    }

    if (exists $o->{absent}) {
	$expat->xpcroak("$o->{absent} can't be absent")
	    if $type =~ m/^(ready|observes|uneasy|destroys)$/;
	$expat->xpcroak("initiator is absent?")
	    if $o->{absent} eq $o->{initiator};
	$expat->xpcroak("absent `$o->{absent}' is left or right?")
	    if $o->{absent} !~ m/^(left|right)$/;
    }

    if ($type eq 'ready') {
	$expat->xpcroak("ready is not initiated")
	    if exists $o->{initiator};
    } elsif ($type =~ m/^(observes|uneasy)$/) {
	# OK
    } else {
	my $i = $o->{initiator} || 'undef';
	$expat->xpcroak("initiator `$i' is left or right?")
	    if $i !~ m/^(left|right)$/;
    }

    $expat->xpcroak("left is missing")
	if !$o->{left};
    $expat->xpcroak("right is missing")
	if !$o->{right};

    $expat->xpcroak("$o->{left} competing with himself/herself")
	if $o->{left} eq $o->{right};
	
    if ($type eq 'impasse') {
	#$expat->xpcroak("impasse to what?")
	#    if !sum(map { exists $o->{$_} } qw(re echo amend absent));
	$expat->xpcroak("choose *one*: re/echo/amend/absent")
	    if sum(map { exists $o->{$_} } qw(re echo amend absent)) != 1;
	my $re = $o->{re} || $o->{echo} || $o->{amend};
	if ($re) {
	    my $aty = $re->{type};
	    $expat->xpcroak("impasse to `$aty'?")
		if $aty =~ m/^(observes|uneasy)$/;
	}
    }

    if (exists $o->{echo}) {
	# are the constraints are wrong?
	# initiator doesn't matter but victim should match?

	$expat->xpcarp("echo without absent")  # echo implies absent
	    unless exists $o->{absent};

	my $re = $o->{echo};

	my $i1 = $re->initiator;
	my $i2 = $o->initiator;
	$expat->xpcroak("echo but initiator switched `$i1'->`$i2'?")
	    if $i1 ne $i2;
	
	$expat->xpcarp("$o->{left} not in echo")
	    if ($o->{left} ne $re->{left} and $o->{left} ne $re->{right});
	$expat->xpcarp("$o->{right} not in echo")
	    if ($o->{right} ne $re->{left} and $o->{right} ne $re->{right});
    }

    if (exists $o->{re}) {
	my $re = $o->{re};
	$expat->xpcroak("can't re readiness")
	    if $re->{type} eq 'ready';
	
	my $i1 = $re->initiator;
	my $i2 = $o->initiator;

	if (!$i1 and !$i2) {
	    $expat->xpcarp("who has initiator?");
	} elsif (defined($i1) xor defined($i2)) {
	    if ($i1) {
		$o->{initiator} = $i1 eq 'left'? 'right':'left';
	    } else {
		$re->{initiator} = $i2 eq 'left'? 'right':'left';
	    }
	} elsif ($i1 eq $i2) {
	    $expat->xpcroak("$i1 kept the initiative")
	}

	$expat->xpcarp($re->initiator." not in re")
	    if $re->initiator ne $o->victim;
	my $v = $re->victim;
	$expat->xpcarp("$v not in re")
	    if $v ne '*' && $v ne $o->initiator;
    }

    if (exists $o->{amend}) {
	my $am = $o->{amend};
	$expat->xpcroak($am->initiator." ne ".$o->initiator)
	    if $am->initiator ne $o->initiator;
	$expat->xpcroak($am->victim." ne ".$o->victim)
	    if $am->victim ne $o->victim;
    }

    if (exists $o->{revoke}) {
	my $oops = $o->{revoke};
	my @open;
	push @open, $oops->{re} if
	    exists $oops->{re};
	for my $z (@open) {
	    $Open{ $z->label } = $z;
	}
    }

    delete $Open{ $o->{amend}->label } if 
	exists $o->{amend};

    if (exists $o->{revoke}) {
	my $label = $o->{revoke}->label;
	if (delete $Open{ $label }) {
	    # OK
	} else {
	    $expat->xpcarp("amend $label");
	}
    }
    if (exists $o->{re}) {
	my $label = $o->{re}->label;
	if (delete $Open{ $label }) {
	    # OK
	} else {
	    my $an = $o->{re};
	    if ($an->{type} eq 'admires' and $an->{before}) {
		# OK
	    } else {
		$expat->xpcarp("amend $label");
	    }
	}
    }

    if (exists $o->{absent}) {
	# OK
    } elsif (exists $o->{closing}) {
	# OK
    } elsif ($type =~ m/^(observes|uneasy|ready)$/) {
	# OK
    } elsif ($type eq 'accepts' and exists $o->{tension}) {
	# OK
    } elsif ($type eq 'admires' and exists $o->{before}) {
	# OK
    } else {
	$Open{ $o->label } = $o;
    }

    $o
}

sub label {
    my ($l) = @_;
    "$l->{dialog_id}.$l->{phrase_id}"
}

sub initiator {  # NUKE? XXX
    my ($o) = @_;
    if (exists $o->{initiator}) {
	$o->{ $o->{initiator} };
    } elsif ($o->{type} =~ m/^(impasse|destroys)$/) {
	$o->{left};
    } else {
	undef
    }
}

sub victim {
    my ($o) = @_;
    my $i = $o->{initiator};
    if ($i) {
	if ($i eq 'left') {
	    $o->{right};
	} else {
	    $o->{left};
	}
    } elsif ($o->{type} =~ m/^(impasse|destroys)$/) {
	$o->{right};
    } else {
	undef
    }
}

sub _accent {
    my ($o) = @_;
    my $hash='';
    for my $phase (qw(before after tension)) {
	next if !exists $o->{$phase};
	my $pc = do {
	    if ($phase eq 'before') { 0; }
	    elsif ($phase eq 'tension') { 1; }
	    else { 2; }
	};
	$hash .= $pc.'='. substr($o->{$phase},0,2);
	last;
    }
    if (exists $o->{intensity}) {
	my $i = $o->{intensity};
	$hash .= do {
	    if ($i eq 'gentle') { 1; }
	    elsif ($i eq 'forceful') { 2; }
	    else { 3 }
	};
    }

    if ($o->{type} =~ m/^(steals|exposes|admires|accepts)$/) {
	$hash .= substr $o->{initiator},0,1;
    }
    if (exists $o->{absent}) {
	$hash .= 'a';
    }
    $hash;
}

sub hash {
    my ($o) = @_;
    my @k;

    push @k, $TypeCode{ $o->{type} };
    if (exists $o->{re}) {
	my $an = $o->{re};
	push @k, 'a='.$TypeCode{ $an->{type} };
	push @k, _accent($an);
    }
    push @k, _accent($o);

    join(':', @k)
}

# This should use more formalized pattern matching instead of
# a mess of if/then statements...? XXX
#
# Otherwise, how to detect multiple matches...?

sub emotion {
    my ($o) = @_;
    my $ty = $o->{type};
    my $re;
    my $aty = '';
    if (exists $o->{re}) {
	$re = $o->{re};
	$aty = $re->{type};
    }
    my $context;  # try to avoid for classification purposes
    my $cty = '';
    if (exists $o->{context}) {
	$context = $o->{context};
	$cty = $context->{type};
    }
    if ($ty eq 'exposes') {
	if ($o->{initiator} eq 'left') {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    'zealous enthusiasm';
		} elsif ($te eq 'relaxed') {
		    'righteousness';
		} else {
		    'meticulous doubt';
		}
	    } elsif (exists $o->{after}) {
		my $te = $o->{after};
		if ($te eq 'focused') {
		    'vindicated / euphoria';
		} elsif ($te eq 'stifled') {
		    'allow the frenzy to dissipate';
		} else { '?' }
	    } else {
		'?';
	    }
	} else {
	    if ($cty eq 'steals') {
		my $te = $o->{tension};
		if ($te eq 'relaxed') {
		    'researching';
		} else { '?' }
	    } elsif (!$aty or $aty eq 'exposes') {
		my $te = $o->{tension};
		if ($te eq 'focused') {
		    'hatred';
		} elsif ($te eq 'relaxed') {
		    'frustration / sheepishly offer gratitude';
		} else {
		    'embarrassment / denial';
		}
	    } elsif ($aty eq 'accepts' and exists $re->{before}) {
		my $te = $o->{tension};
		if ($te eq 'focused') {
		    '?';
		} elsif ($te eq 'relaxed') {
		    'admit mistake';
		} else {
		    'try to cover up mistake';
		}
	    } else { '?' }
	}
    } elsif ($ty eq 'steals') {
	if ($o->{initiator} eq 'left') {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    'anxiety';
		} elsif ($te eq 'relaxed') {
		    'inwardly uncaring';
		} else {
		    'cool and calculating';
		}
	    } elsif (exists $o->{after}) {
		my $te = $o->{after};
		if ($te eq 'focused') {
		    'drunk with accomplishment';
		} elsif ($te eq 'stifled') {
		    'strain for sobriety';
		} else { '?' }
	    } else {
		'?';
	    }
	} else {
	    my $te = $o->{tension};
	    if (!$aty or $aty eq 'steals') {
		if ($te eq 'focused') {
		    'angry at thief';
		} elsif ($te eq 'stifled') {
		    'angry with his/her self';
		} else { '?' }
	    } elsif ($aty eq 'accepts' and exists $re->{before}) {
		if ($te eq 'focused') {
		    'accusal';
		} elsif ($te eq 'stifled') {
		    'grudging compliance'
		} else { '?' }
	    } elsif ($aty eq 'exposes') {
		if ($te eq 'focused') {
		    'doubtless righteousness';
		} elsif ($te eq 'relaxed') {
		    'purified desire';
		} else { '?' }
	    } else { '?' }
	}
    } elsif ($ty eq 'admires') {
	if ($o->{initiator} eq 'right') {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    'haughty / arrogant';
		} elsif ($te eq 'relaxed') {
		    'humble confidence';
		} else {
		    'seek the means to radiate purity unconditionally'
		}
	    } else {
		my $te = $o->{after};
		if ($te eq 'relaxed') {
		    'reduces the margin for error';
		} else { '?' }
	    }
	} else {
	    my $te = $o->{tension};
	    if (!$aty or $aty eq 'admires') {
		if ($te eq 'focused') {
		    'offer service';
		} elsif ($te eq 'relaxed') {
		    'made whole';
		} else {
		    'whether to become hooked / stuggle to discriminate';
		}
	    } else { '?' }
	}
    } elsif ($ty eq 'accepts') {
	if ($o->{initiator} eq 'right') {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    'demand';
		} elsif ($te eq 'relaxed') {
		    'neutral probe';
		} else {
		    'hoping to be understood';
		}
	    } elsif (exists $o->{after}) {
		my $te = $o->{after};
		if ($te eq 'focused') {
		    'false sense of security';
		} elsif ($te eq 'relaxed') {
		    'relief';
		} else {
		    if (!$aty) {
			'?'
		    } elsif ($aty eq 'accepts' and exists $re->{before} and
			$re->{before} eq 'focused') {
			'dubious about a candid assertion';
		    } elsif ($aty eq 'observes' and
			     $re->{intensity} eq 'gentle') {
			'dubious about a brush-off reply';
		    } else {
			'dubious';
		    }
		}
	    } else {
		'?';
	    }
	} else {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    if ($aty eq 'impasse' and 
			$re->{tension} eq 'relaxed') {
			'guess the riddle';
		    } else {
			'confidence';
		    }
		} elsif ($te eq 'relaxed') {
		    'probe of agreement'
		} else { '?' }
	    } elsif (exists $o->{after}) {
		my $te = $o->{after};
		if ($aty eq 'accepts') {
		    if ($te eq 'focused') {
			'balloon enthusiasm out of control';
		    } elsif ($te eq 'stifled') {
			'dissipate delight';
		    } else { '?' }
		} else { '?' }
	    } else {
		my $te = $o->{tension};
		if (!$aty) {
		    if ($te eq 'focused') {
			'it is done';
		    } elsif ($te eq 'relaxed') {
			'yes exactly';
		    } else {
			'?';
		    }
		} elsif ($aty eq 'accepts') {
		    if ($te eq 'focused') {
			'seems to accept';
		    } elsif ($te eq 'relaxed') {
			'embraces wish';
		    } else { '?' }
		} elsif ($aty eq 'impasse') {
		    if ($te eq 'focused') {
			'cheer impasse';
		    } elsif ($te eq 'relaxed') {
			'acknowledge impasse';
		    } else {
			'exasperated / concede';
		    }
		} elsif ($aty eq 'steals') {
		    if ($te eq 'focused') {
			'convert to admiration';
		    } elsif ($te eq 'relaxed') {
			'force humility';
		    } else {
			'convert to uneasy';
		    }
		} elsif ($aty eq 'exposes') {
		    if ($te eq 'focused') {
			'whoops!';
		    } elsif ($te eq 'relaxed') {
			'oh, ah / silence';
		    } else {
			'please let me try to explain';
		    }
		} else { '?' }
	    }
	}
    } elsif ($ty eq 'destroys') {
	'death';
    } elsif ($ty eq 'uneasy') {
	if (!$aty) {
	    my $in = $o->{intensity};
	    if ($in eq 'gentle') {
		'nervousness';
	    } elsif ($in eq 'forceful') {
		'discomfort';
	    } else {
		'sleepless agony';
	    }
	} elsif ($aty eq 'accepts' and exists $re->{before} and
		 $re->{before} eq 'focused' and
		 $re->{initiator} eq 'right') {
	    my $in = $o->{intensity};
	    if ($in eq 'gentle') {
		'caught red-handed';
	    } else { '?' }
	} elsif ($aty eq 'accepts' and exists $re->{before} and
		 $re->{before} eq 'focused' and
		 $re->{initiator} eq 'left') {
	    my $in = $o->{intensity};
	    if ($in eq 'gentle') {
		'suspicious';
	    } elsif ($in eq 'forceful') {
		'react to fear by striving for harmony';
	    } else {
		'faint under pressure';
	    }
	} else { '?' }
    } elsif ($ty eq 'observes') {
	my $in = $o->{intensity};
	if (!$aty) {
	    if ($in eq 'gentle') {
		'excitement';
	    } elsif ($in eq 'forceful') {
		'exciting test of self-control';
	    } else {
		'challenge of emotional insight';
	    }
	} elsif ($aty eq 'accepts') {
	    if ($in eq 'gentle') {
		'seems to accept';
	    } elsif ($in eq 'forceful') {
		'(silence)';
	    } else {
		'blasts her with silent force'
	    }
	} elsif ($aty eq 'exposes' and ($re->{before}||'?') eq 'focused') {
	    if ($in eq 'gentle') {
		'threaten';
	    } elsif ($in eq 'forceful') {
		'narrowly avoid threat';
	    } else {
		'?';
	    }
	} elsif ($aty eq 'impasse' and $re->{tension} eq 'focused') {
	    if ($in eq 'gentle') {
		'amused by impasse';
	    } else { '?' }
	} elsif ($aty eq 'impasse' and $re->{tension} eq 'relaxed') {
	    if ($in eq 'gentle') {
		'amused by counteroffer';
	    } else { '?' }
	} else { '?' }
    } elsif ($ty eq 'ready') {
	my $in = $o->{intensity};
	if ($in eq 'gentle') {
	    'emotional turmoil / paralysis';
	} elsif ($in eq 'forceful') {
	    'limbo';
	} else {
	    'Ready!';
	}
    } elsif ($ty eq 'impasse') {
	my $te = $o->{tension};
	if (exists $o->{absent}) {
	    if ($te eq 'focused') {
		'frustrated by absence';
	    } elsif ($te eq 'relaxed') {
		'signal absence';
	    } else {
		'mourn absence';
	    }
	} else {
	    if ($te eq 'focused') {
		'stubborn / frustrated / indignant';
	    } elsif ($te eq 'relaxed') {
		'probe / sincere and balanced concern';
	    } else {
		'separation';
	    }
	}
    } else { '?' }
}

1;
