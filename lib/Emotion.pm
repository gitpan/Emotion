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
our $VERSION = '0.16';

our $WarnLevel = 2;
our $Stem;
our $DialogID;
our %Link;  # resolve symbolic links
our %Open;  # by label

my ($warn, $error);

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

    if ($WarnLevel == 0) {        # silent
	$error = 'ignore';
	$warn = 'ignore';
    } elsif ($WarnLevel == 1) {   # permissive
	$error = 'xpcarp';
	$warn = 'xpcarp';
    } elsif ($WarnLevel == 2) {   # normal
	$error = 'xpcroak';
	$warn = 'xpcarp';
    } elsif ($WarnLevel == 3) {   # pedantic
	$error = 'xpcroak';
	$warn = 'xpcroak';
    } else {
	die '$WarnLevel='.$WarnLevel;
    }
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

package XML::Parser::Expat;
sub ignore {}

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
	if ($id !~ m/^\w+$/) {
	    $expat->$error("Link `$id' doesn't match \\w+");
	} else {
	    if (exists $Link{$id}) {
		my $try = $id;
		++$try while exists $Link{$try};
		$expat->$error("Link collision with `$id' at talk $Link{$id}{dialog_id}, try `$try'");
	    } else {
		$Link{$id} = $o;
	    }
	}
    }

    for (qw(reply answer)) {
	if (exists $o->{$_}) {
	    $expat->$error("`$_' renamed to `re'");
	    $o->{re} = delete $o->{$_};
	}
    }

    for my $kind (qw(re echo revoke amend context)) {
	next if !exists $o->{$kind};
	my $to = $o->{$kind};
	$o->{$kind} = $Link{$to};
	if (!$o->{$kind}) {
	    $expat->$error("broken $kind link `$to'");
	    delete $o->{$kind};
	    last;
	}
    }

    # tension & intensity
    if ($type =~ m/^(steals|exposes|admires|accepts)$/) { #spin
	my $before = $o->{before};
	my $after = $o->{after};
	my $tension = $o->{tension};
	if (exists $o->{intensity}) {
	    $expat->$error("rename intensity to tension");
	    $tension = $o->{tension} = delete $o->{intensity};
	}
	if (defined($before) + defined($after) + defined($tension) != 1) {
	    $expat->$error("before or after or intensity?");
	    delete $o->{$_} for qw(before after);
	    $o->{tension} = 'relaxed';
	}
	my $i = $before || $after || $tension;
	if ($i !~ m/^(focused|relaxed|stifled)$/) {
	    $expat->$error("$i?");
	}
    } elsif ($type eq 'impasse') {
	my $tension = $o->{tension};
	if (exists $o->{intensity}) {
	    $expat->$error("rename intensity to tension");
	    $tension = $o->{tension} = delete $o->{intensity};
	}
	$expat->$error("tension=focused|relaxed|stifled?")
	    if !$tension;
    } elsif ($type =~ m/^(uneasy|observes|ready)$/) {
	my $i = $o->{intensity};
	$expat->$error("intensity=$i? (gentle|forceful|extreme)")
	    if !$i || $i !~ m/^(gentle|forceful|extreme)$/;
    } else { #destroys
	for (qw(intensity tension)) {
	    if (exists $o->{$_}) {
		$expat->$warn("no `$_' needed");
	    }
	}
    }
	
    if (exists $o->{initiative}) {
	$expat->$error("initiative renamed to initiator");
	$o->{initiator} = delete $o->{initiative};
    }

    if ($type =~ m/^(impasse|destroys)$/) {
	if (exists $o->{initiator}) {
	    if (exists $o->{left} || $o->{initiator} eq 'left') {
		$expat->$warn("$type initiator is always left");
	    } else {
		$o->{left} = $o->{initiator};
	    }
	}
	$o->{initiator} = 'left';
    } elsif (exists $o->{initiator}) {
	my $i = $o->{initiator};
	if (exists $o->{left} xor exists $o->{right}) {
	    if ($i =~ m/^(left|right)$/) {
		$expat->$error("initiator is who?");
		$i = "someone";
	    }
	    if (exists $o->{left}) {
		$o->{right} = $i;
		$o->{initiator} = 'right'
	    } else {
		$o->{left} = $i;
		$o->{initiator} = 'left';
	    }
	} else {
	    if ($i !~ m/^(left|right)$/) {
		$expat->$error("initiator `$i' is left or right?");
		$i = 'left';
	    }
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
	if ($type =~ m/^(ready|observes|uneasy|destroys)$/) {
	    $expat->$error("$o->{absent} can't be absent");
	    delete $o->{absent};
	} elsif ($o->{absent} eq $o->{initiator}) {
	    $expat->$error("initiator is absent?");
	    delete $o->{absent};
	} elsif ($o->{absent} !~ m/^(left|right)$/) {
	    $expat->$error("absent `$o->{absent}' is left or right?");
	    delete $o->{absent};
	}	    
    }

    if ($type eq 'ready') {
	if (exists $o->{initiator}) {
	    $expat->$warn("ready is not initiated");
	    delete $o->{initiator};
	}
    } elsif ($type =~ m/^(observes|uneasy)$/) {
	# OK
    } else {
	my $i = $o->{initiator} || 'undef';
	if ($i !~ m/^(left|right)$/) {
	    $expat->$error("initiator `$i' is left or right?");
	    $o->{initiator} = 'left';
	}
    }

    for (qw(left right)) {
	if (!$o->{$_}) {
	    $expat->$error("`$_' is missing");
	    $o->{$_} = 'someone';
	}
    }

    if ($o->{left} eq $o->{right}) {
	$expat->$error("$o->{left} competing with himself/herself");
	$o->{right} = 'sometwo';
    }
	
    if ($type eq 'impasse') {
	$expat->$warn("choose *one*: re/echo/amend/absent")
	    if sum(map { exists $o->{$_} } qw(re echo amend absent)) != 1;
	my $re = $o->{re} || $o->{echo} || $o->{amend};
	if ($re) {
	    my $aty = $re->{type};
	    $expat->$error("impasse to `$aty'?")
		if ($aty =~ m/^(observes|ready)$/);
	}
    }

    if (exists $o->{echo}) {
	# are the constraints okay?
	# initiator doesn't matter but victim should match?

	my $re = $o->{echo};

	my $v1 = $re->victim;
	my $v2 = $o->victim;
	$expat->$error("echo victim `$v1' changed to `$v2'?")
	    if $v1 ne $v2;
    }

    if (exists $o->{re}) {
	my $re = $o->{re};
	$expat->$error("can't re readiness")
	    if $re->{type} eq 'ready';
	
	my $i1 = $re->initiator;
	my $i2 = $o->initiator;

	if (!$i1 and !$i2) {
	    $expat->$warn("who is initiator?");
	} elsif (defined($i1) xor defined($i2)) {
	    if ($i1) {
		$o->{initiator} = $i1 eq $o->{left}? 'right':'left';
		# why always left? XXX
		# warn $o->label." to $o->{initiator} due to ".$re->label;
	    } else {
		die $re->label." due to ".$o->label;
		$re->{initiator} = $i2 eq $re->{left}? 'right':'left';
	    }
	} elsif ($i1 eq $i2) {
	    $expat->$error("$i1 kept the initiative")
	}

	$i1 = $re->initiator;
	my $v1 = $re->victim;
	$i2 = $o->initiator;
	my $v2 = $o->victim;

	$expat->$warn("`$i1' (not `$v2') in re")
	    if $v2 ne '*' && $i1 ne $v2;
	$expat->$warn("`$i2' (not `$v1') in re")
	    if $v1 ne '*' && $v1 ne $i2;
	$expat->$warn("$i1 is the sole initiator; you want amend?")
	    if $i1 eq $i2;
    }

    if (exists $o->{amend}) {
	my $am = $o->{amend};
	my $i1 = $am->initiator;
	my $i2 = $o->initiator;

	if (!$i1 and !$i2) {
	    $expat->$error("who is initiator?");
	} elsif (defined($i1) xor defined($i2)) {
	    if ($i1) {
		$o->{initiator} = $i1 eq $o->{left}? 'left':'right';
	    } else {
		$am->{initiator} = $i2 eq $am->{left}? 'left':'right';
	    }
	} elsif ($i1 ne $i2) {
	    $expat->$error("$i1 didn't keep the initiative")
	}

	$expat->$error($am->victim." ne ".$o->victim)
	    if $am->victim ne '*' && $am->victim ne $o->victim;
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
	my $re = $o->{revoke};
	my $label = $re->label;
	if (delete $Open{ $label }) {
	    # OK
	} elsif ($re->{type} =~ /^(steals|exposes)$/ and $re->{tension}) {
	    # OK
	} else {
	    $expat->$warn("ambiguous amend revoke=$label");
	}
    }
    if (exists $o->{echo}) {
	my $ec = $o->{echo};
	my $label = $ec->label;
	if ($o->initiator eq $ec->initiator) {
	    if (delete $Open{ $label }) {
		# OK
	    } else {
		$expat->$warn("ambiguous amend echo=$label");
	    }
	} else {
	    # joinder OK
	}
    }
    if (exists $o->{re}) {
	my $re = $o->{re};
	my $label = $re->label;
	if (delete $Open{ $label }) {
	    # OK
	} else {
	    if ($re->{type} eq 'admires' and $re->{before}) {
		# OK
	    } elsif ($type eq 'admires' and $o->{before}) {
		# OK
	    } elsif ($re->{type} =~ m/^(accepts|steals)$/ and $re->{tension}) {
		# OK
	    } elsif ($re->{type} =~ m/^(observes|uneasy)$/) {
		# OK
	    } elsif ($re->victim eq '*') {
		# OK
	    } else {
		$expat->$warn("ambiguous amend re=$label");
	    }
	}
    }

    if (exists $o->{absent}) {
	# OK
    } elsif ($type =~ m/^(observes|uneasy|ready)$/) {
	# OK
    } elsif ($type =~ m/(accepts|steals|admires|exposes)$/ and
	     (exists $o->{tension} or exists $o->{after})) {
	# OK
    } elsif ($type eq 'admires' and exists $o->{before} and
	     $o->{initiator} eq 'right') {
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
    my $rety = '';
    if (exists $o->{re}) {
	$re = $o->{re};
	$rety = $re->{type};
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
	    } elsif (!$rety or $rety eq 'exposes') {
		my $te = $o->{tension};
		if ($te eq 'focused') {
		    'hatred';
		} elsif ($te eq 'relaxed') {
		    'frustration / sheepishly offer gratitude';
		} else {
		    'embarrassment / denial';
		}
	    } elsif ($rety eq 'accepts' and exists $re->{before}) {
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
	    if (!$rety or $rety eq 'steals') {
		if ($te eq 'focused') {
		    'angry at thief';
		} elsif ($te eq 'stifled') {
		    'angry with his/her self';
		} else {
		    'detached indifference';
		}
	    } elsif ($rety eq 'accepts' and exists $re->{before}) {
		if ($te eq 'focused') {
		    'accusal';
		} elsif ($te eq 'stifled') {
		    'grudging compliance'
		} else {
		    'detached indifference';
		}
	    } elsif ($rety eq 'exposes') {
		if ($te eq 'focused') {
		    'doubtless righteousness';
		} elsif ($te eq 'relaxed') {
		    'purified desire';
		} else { '?' }
	    } elsif ($rety eq 'impasse' and $re->{tension} eq 'focused') {
		my $te = $o->{tension};
		if ($te eq 'focused') {
		    'belligerent / shy';
		} elsif ($te eq 'relaxed') {
		    '(wince / shy / belligerent / silent)';
		} else {
		    'crumpled / shy';
		}
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
		    'humble / meek';
		}
	    } else {
		my $te = $o->{after};
		if ($te eq 'relaxed') {
		    'reduces the margin for error';
		} else { '?' }
	    }
	} else {
	    if (exists $o->{before}) {
		my $te = $o->{before};
		if ($te eq 'focused') {
		    'mocking';
		} elsif ($te eq 'relaxed') {
		    '?';
		} else {
		    'euphemism';
		}
	    } elsif (exists $o->{after}) {
		'?';
	    } else {
		my $te = $o->{tension};
		if (!$rety or $rety eq 'admires') {
		    if ($te eq 'focused') {
			'awe / offer service';
		    } elsif ($te eq 'relaxed') {
			'made whole';
		    } else {
			'whether to become hooked / stuggle to discriminate';
		    }
		} else { '?' }
	    }
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
		    if (!$rety) {
			'?'
		    } elsif ($rety eq 'accepts' and exists $re->{before} and
			$re->{before} eq 'focused') {
			'dubious about a candid assertion';
		    } elsif ($rety eq 'observes' and
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
		    if ($rety eq 'impasse' and 
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
		if ($rety eq 'accepts') {
		    if ($te eq 'focused') {
			'balloon enthusiasm out of control';
		    } elsif ($te eq 'stifled') {
			'dissipate delight';
		    } else { '?' }
		} else { '?' }
	    } else {
		my $te = $o->{tension};
		if (!$rety) {
		    if ($te eq 'focused') {
			'it is done';
		    } elsif ($te eq 'relaxed') {
			'yes exactly';
		    } else {
			'?';
		    }
		} elsif ($rety eq 'accepts') {
		    if ($te eq 'focused') {
			'seems to accept';
		    } elsif ($te eq 'relaxed') {
			'embraces wish';
		    } else { '?' }
		} elsif ($rety eq 'impasse') {
		    if ($te eq 'focused') {
			'cheer impasse';
		    } elsif ($te eq 'relaxed') {
			'acknowledge impasse';
		    } else {
			'exasperated / concede';
		    }
		} elsif ($rety eq 'steals') {
		    if ($te eq 'focused') {
			'convert to admiration';
		    } elsif ($te eq 'relaxed') {
			'humbled';
		    } else {
			'convert to uneasy';
		    }
		} elsif ($rety eq 'exposes') {
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
	if (!$rety) {
	    my $in = $o->{intensity};
	    if ($in eq 'gentle') {
		'nervousness';
	    } elsif ($in eq 'forceful') {
		'discomfort';
	    } else {
		'sleepless agony';
	    }
	} elsif ($rety eq 'accepts' and exists $re->{before} and
		 $re->{before} eq 'focused' and
		 $re->{initiator} eq 'right') {
	    my $in = $o->{intensity};
	    if ($in eq 'gentle') {
		'caught red-handed';
	    } else { '?' }
	} elsif ($rety eq 'accepts' and exists $re->{before} and
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
	if (!$rety) {
	    if ($in eq 'gentle') {
		'excitement';
	    } elsif ($in eq 'forceful') {
		'exciting test of self-control';
	    } else {
		'challenge of emotional insight';
	    }
	} elsif ($rety eq 'accepts') {
	    if ($in eq 'gentle') {
		'seems to accept';
	    } elsif ($in eq 'forceful') {
		'(silence)';
	    } else {
		'blasts her with silent force'
	    }
	} elsif ($rety eq 'exposes' and ($re->{before}||'?') eq 'focused') {
	    if ($in eq 'gentle') {
		'threaten';
	    } elsif ($in eq 'forceful') {
		'narrowly avoid threat';
	    } else {
		'?';
	    }
	} elsif ($rety eq 'impasse' and $re->{tension} eq 'focused') {
	    if ($in eq 'gentle') {
		'amused by impasse';
	    } else { '?' }
	} elsif ($rety eq 'impasse' and $re->{tension} eq 'relaxed') {
	    if ($in eq 'gentle') {
		'amused by counteroffer';
	    } else { '?' }
	} else { '?' }
    } elsif ($ty eq 'ready') {
	my $in = $o->{intensity};
	if ($in eq 'gentle') {
	    'paralysis / emotional turmoil';
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
		'indignant / frustrated / stubborn';
	    } elsif ($te eq 'relaxed') {
		'probe / sincere and balanced concern';
	    } else {
		'procrastinate / separation';
	    }
	}
    } else { '?' }
}

1;
