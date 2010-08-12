package DBIx::Class::Tree::Mobius;
# ABSTRACT: Manage trees of data using the nested intervals with continued fraction encoding

use strict;
use warnings;

use base qw/DBIx::Class/;

__PACKAGE__->mk_classdata( 'parent_virtual_column' => 'parent' );

__PACKAGE__->mk_classdata( '_mobius_a_column' => 'mobius_a' );
__PACKAGE__->mk_classdata( '_mobius_b_column' => 'mobius_b' );
__PACKAGE__->mk_classdata( '_mobius_c_column' => 'mobius_c' );
__PACKAGE__->mk_classdata( '_mobius_d_column' => 'mobius_d' );
__PACKAGE__->mk_classdata( '_left_column' => 'left' );
__PACKAGE__->mk_classdata( '_right_column' => 'right' );
__PACKAGE__->mk_classdata( '_is_inner_column' => 'inner' );

sub add_mobius_tree_columns {
    my $class = shift;
    my %column_names = @_;

    foreach my $name (qw/ mobius_a mobius_b mobius_c mobius_d left right is_inner /) {
        next unless exists $column_names{$name};
        my $accessor = "_${name}_column";
        $class->$accessor( $column_names{$name} );
    }

    $class->add_columns(
        $class->_mobius_a_column => { data_type => 'INT', size => 11, is_nullable => 1, extra => { unsigned => 1} },
        $class->_mobius_b_column => { data_type => 'INT', size => 11, is_nullable => 1, extra => { unsigned => 1} },
        $class->_mobius_c_column => { data_type => 'INT', size => 11, is_nullable => 1, extra => { unsigned => 1} },
        $class->_mobius_d_column => { data_type => 'INT', size => 11, is_nullable => 1, extra => { unsigned => 1} },
        $class->_left_column => { data_type => 'FLOAT', is_nullable => 0, default_value => 1, extra => { unsigned => 1} },
        $class->_right_column => { data_type => 'FLOAT', is_nullable => 1, default_value => undef, extra => { unsigned => 1} },
        $class->_is_inner_column => { data_type => "BOOLEAN", default_value => 0, is_nullable => 0 },
        );

    $class->add_unique_constraint( $class->_mobius_a_column . $class->_mobius_c_column, [ $class->_mobius_a_column, $class->_mobius_c_column ] );

    if ($class =~ /::([^:]+)$/) {

        $class->belongs_to( 'parent' => $1 => {
            "foreign.".$class->_mobius_a_column => "self.".$class->_mobius_b_column,
            "foreign.".$class->_mobius_c_column => "self.".$class->_mobius_d_column,
        });

        $class->has_many( 'children' => $1 => {
            "foreign.".$class->_mobius_b_column => "self.".$class->_mobius_a_column,
            "foreign.".$class->_mobius_d_column => "self.".$class->_mobius_c_column,
        }, { cascade_delete => 0 });
      
    }

}

sub root_cond {
    my $self = shift;
    return ( $self->_mobius_b_column => undef, $self->_mobius_d_column => undef );
}

sub inner_cond {
    my $self = shift;
    return $self->_is_inner_column => 1 ;
}

sub leaf_cond {
    my $self = shift;
    return $self->_is_inner_column => 0 ;
}

sub _rational {
    my $i = shift;

    return unless ($i);
    return ($i, 1) unless (scalar @_ > 0);

    my ($num, $den) = _rational(@_);
    return ($num * $i + $den, $num);
}

sub _euclidean {
    my ($a, $c) = @_;

    return unless ($c);
    my $res = $a % $c;
    return $res == 0 ? int($a / $c) : (int($a / $c), _euclidean($c, $res));
}

sub _mobius {
    my $i = shift;

    return (1, 0, 0, 1) unless ($i);
    my ($a, $b, $c, $d) = _mobius(@_);
    return ($i * $a + $c, $i * $b + $d, $a, $b);
}

sub _mobius_encoding {
    my ($a, $b, $c, $d) = _mobius(@_);
    return wantarray ? ($a, $b, $c, $d) : sprintf("(${a}x + $b) / (${c}x + $d)");
}

sub _mobius_path {
    my ($a, $b, $c, $d) = @_;
    my @path = _euclidean($a, $c);
    return wantarray ? @path : join('.', @path);
}

sub _left_right {
    my ($a, $b, $c, $d) = @_;
    my ($x, $y) = (($a+$b)/($c+$d), $a / $c);
    my ($left, $right) = $x > $y ? ($y, $x) : ($x, $y);
    return wantarray ? ($left, $right) : sprintf("l=%.3f, r=%.3f", $left, $right);
}

sub new {
    my ($class, $attrs) = @_;
    $class = ref $class if ref $class;
  
    if (my $parent = delete($attrs->{$class->parent_virtual_column})) {
        # store aside explicitly parent
        my $new = $class->next::method($attrs);
        $new->{_explicit_parent} = $parent;
        return $new;
    } else {
        return $class->next::method($attrs);
    }
}

# always use the leftmost index available

sub _available_mobius_index {
    my @children = @_;

    my $count = scalar @children + 2;
    foreach my $child (@children) {
        my @mpath = $child->mobius_path();
        my $index = pop @mpath;
        last if ($count  > $index);
        $count--;
    }
    return $count;
}

sub available_mobius_index {
    my $self = shift;
    return _available_mobius_index( $self->children()->search({}, { order_by => $self->_mobius_a_column. ' DESC' } ) );
}

sub insert {
    my $self = shift;

    if (exists $self->{_explicit_parent}
        and my $parent = $self->result_source->resultset->find($self->{_explicit_parent}) ) {

        my ($a, $b, $c, $d, $left, $right) = $parent->child_encoding( $parent->available_mobius_index );

        $self->store_column( $self->_mobius_a_column => $a );
        $self->store_column( $self->_mobius_b_column => $b );
        $self->store_column( $self->_mobius_c_column => $c );
        $self->store_column( $self->_mobius_d_column => $d );
        $self->store_column( $self->_left_column => $left );
        $self->store_column( $self->_right_column => $right );

        my $r = $self->next::method(@_);
        $parent->update({ $self->_is_inner_column => 1 } );
        return $r;

    } else {  # attaching to root

        my $x = _available_mobius_index( $self->result_source->resultset->search( { $self->root_cond } )->search({}, { order_by => $self->_mobius_a_column. ' DESC' } ) );

        $self->store_column( $self->_mobius_a_column => $x );
        $self->store_column( $self->_mobius_c_column => 1 );
        # normal value are b => 1 and c => 0 but it cannot work for SQL join
        $self->store_column( $self->_mobius_b_column => undef );
        $self->store_column( $self->_mobius_d_column => undef );
        $self->store_column( $self->_left_column => $x );
        $self->store_column( $self->_right_column => $x + 1 );
        return $self->next::method(@_);

    }

}

sub mobius_path {
    my $self = shift;
    my ($b, $d) =  ($self->get_column($self->_mobius_b_column), $self->get_column($self->_mobius_d_column));
    my @path = _mobius_path(
        $self->get_column($self->_mobius_a_column), defined $b ? $b : 1,
        $self->get_column($self->_mobius_c_column), defined $d ? $d : 0,
    );
    return wantarray ? @path : join('.', @path);
}

sub child_encoding {
    my $self = shift;
    my $x = shift;
    my ($pb, $pd) =  ($self->get_column($self->_mobius_b_column), $self->get_column($self->_mobius_d_column));
    my ($a, $b, $c, $d) = (
        $self->get_column($self->_mobius_a_column) * $x + ( defined $pb ? $pb : 1),
        $self->get_column($self->_mobius_a_column),
        $self->get_column($self->_mobius_c_column) * $x + ( defined $pd ? $pd : 0),
        $self->get_column($self->_mobius_c_column)
    );
    return wantarray ? ($a, $b, $c, $d, _left_right($a, $b, $c, $d)) : sprintf("(${a}x + $b) / (${c}x + $d)");
}

sub root {
    my $self = shift;
    return $self->result_source->resultset->search( { $self->root_cond } )->search({
        $self->result_source->resultset->current_source_alias.'.'.$self->_left_column => { '<' => $self->get_column($self->_right_column) },
        $self->result_source->resultset->current_source_alias.'.'.$self->_right_column => { '>' => $self->get_column($self->_left_column) },
    });
}

sub is_root {
    my $self = shift;
    return $self->parent ? 0 : 1;
}

sub is_inner {
    my $self = shift;
    return $self->get_column($self->_is_inner_column) ? 1 : 0;
}

sub is_branch {
    my $self = shift;
    return ($self->parent && $self->get_column($self->_is_inner_column)) ? 1 : 0;
}

sub is_leaf {
    my $self = shift;
    return $self->get_column($self->_is_inner_column) ? 0 : 1;
}

sub siblings {
    my $self = shift;
    if (my $parent = $self->parent) {
        return $parent->children->search({
            -or => {
                $self->result_source->resultset->current_source_alias.'.'.$self->_mobius_a_column => { '!=' => $self->get_column($self->_mobius_a_column) },
                $self->result_source->resultset->current_source_alias.'.'.$self->_mobius_c_column => { '!=' => $self->get_column($self->_mobius_c_column) },
            },
        });
    } else {
        return $self->result_source->resultset->search({
            -or => {
                $self->result_source->resultset->current_source_alias.'.'.$self->_mobius_a_column => { '!=' => $self->get_column($self->_mobius_a_column) },
                $self->result_source->resultset->current_source_alias.'.'.$self->_mobius_c_column => { '!=' => $self->get_column($self->_mobius_c_column) },
            },
            $self->result_source->resultset->current_source_alias.'.'.$self->_mobius_b_column => undef,
            $self->result_source->resultset->current_source_alias.'.'.$self->_mobius_d_column => undef
         });
    }
}

sub leaf_children {
    my $self = shift;
    return $self->children->search({ $self->result_source->resultset->current_source_alias.'.'.$self->_is_inner_column => 0 });
}

sub inner_children {
    my $self = shift;
    return $self->children->search({ $self->result_source->resultset->current_source_alias.'.'.$self->_is_inner_column => 1 });
}

sub descendants {
    my $self = shift;

    return $self->result_source->resultset->search({
        $self->result_source->resultset->current_source_alias.'.'.$self->_left_column => { '>' => $self->get_column($self->_left_column) },
        $self->result_source->resultset->current_source_alias.'.'.$self->_right_column => { '<' => $self->get_column($self->_right_column) },
    });
}

sub leaves {
    my $self = shift;
    return $self->descendants->search({ $self->result_source->resultset->current_source_alias.'.'.$self->_is_inner_column => 0 });
}

sub inner_descendants {
    my $self = shift;

    return $self->descendants->search({ $self->result_source->resultset->current_source_alias.'.'.$self->_is_inner_column => 1 });
}

sub ancestors {
    my $self = shift;
        
    return $self->result_source->resultset->search({
        -and => {
            $self->result_source->resultset->current_source_alias.'.'.$self->_left_column => { '<' => $self->get_column($self->_left_column) },
            $self->result_source->resultset->current_source_alias.'.'.$self->_right_column => { '>' => $self->get_column($self->_right_column) },
        },
        $self->result_source->resultset->current_source_alias.'.'.$self->_left_column => { '<' => $self->get_column($self->_right_column) },
        $self->result_source->resultset->current_source_alias.'.'.$self->_right_column => { '>' => $self->get_column($self->_left_column) },
        $self->result_source->resultset->current_source_alias.'.'.$self->_mobius_a_column => { '!=' => $self->get_column($self->_mobius_a_column) },
        $self->result_source->resultset->current_source_alias.'.'.$self->_mobius_c_column => { '!=' => $self->get_column($self->_mobius_c_column) },
    },{ order_by => $self->_left_column.' DESC' });
}

sub ascendants { return shift(@_)->ancestors(@_) }

sub attach_child {
    my $self = shift;
    my $child = shift;

    my ($a, $b, $c, $d, $left, $right) = $self->child_encoding( $self->available_mobius_index );

    my @grandchildren = $child->children()->all();
    foreach my $grandchild (@grandchildren) {
        $grandchild->update( { $self->_mobius_b_column => undef, $self->_mobius_d_column => undef });
    }

    $child->update({ 
        $self->_mobius_a_column => $a,
        $self->_mobius_b_column => $b,
        $self->_mobius_c_column => $c,
        $self->_mobius_d_column => $d,
        $self->_left_column => $left,
        $self->_right_column => $right,
    });

    foreach my $grandchild (@grandchildren) {
        $child->attach_child( $grandchild );
    }

}


1;

=head1 SYNOPSIS

Create a table for your tree data with the 7 special columns used by Tree::Mobius.
By default, these columns are mobius_a mobius_b mobius_b and mobius_d (integer),
left and right (float) and inner (boolean). See add_mobius_tree_columns to change
the default names.

  CREATE TABLE employees (
    name TEXT NOT NULL
    mobius_a integer(11) unsigned,
    mobius_b integer(11) unsigned,
    mobius_c integer(11) unsigned,
    mobius_d integer(11) unsigned,
    left FLOAT unsigned NOT NULL DEFAULT '1',
    right FLOAT unsigned,
    inner boolean NOT NULL DEFAULT '0',
  );

In your Schema or DB class add Tree::Mobius to the top of the component list.

  __PACKAGE__->load_components(qw( Tree::Mobius ... ));

Call add_mobius_tree_columns.

  package My::Employee;
  __PACKAGE__->add_mobius_tree_columns();

Thats it, now you can create and manipulate trees for your table.

  #!/usr/bin/perl
  use My::Employee;
  
  my $big_boss = My::Employee->create({ name=> 'larry W.' });
  my $boss = My::Employee->create({ name=> 'John Doe' });
  my $employee = My::Employee->create({ name=> 'No one' });
  
  $big_boss->attach_child( $boss );
  $boss->attach_child( $employee );

=head1 DESCRIPTION

This module provides methods for working with nested intervals with continued fraction encoding trees. This model is a way of representing hierarchical information in a database. 
This takes a complementary approach of both the 'Nested Sets' model and the 'Materialized Path' model.

The implementation has been heavily inspired by a Vadim Tropashko's article available online at http://arxiv.org/pdf/cs.DB/0402051 .

The nested interval implementation has the same advantages that 'Nested Set' model over the 'Adjacency List' model (obtain all
descendants require only one query and rather than recursive queries). But unlike 'Nested Set' model, the encoding is not volatile, it uses
integer numbers more economically and it doesn't need to be recalculate when a new node is inserted into a tree. Plus, there is no
difficulties associated with querying ancestors or to calculate the path to root of a given node (thus this model is somewhat also
equivalent to the 'Materialized Path' model).

The trade-off of this implementation is the use of 7 SQL columns to encode each node. 

This implementation allows you to have several trees in your database.

=head1 METHODS

=head2 add_mobius_tree_columns

Declare the name of the columns for tree encoding and add them to the schema.

None of these columns should be modified outside if this module.

Multiple trees are allowed in the same table, each tree will have a unique value in the mobius_a_column.

=head2 attach_child

Attach a new child to a node.

If the child has descendants, the entire sub-tree is moved recursively.

=head2 insert

This method is an override of DBIx::Class' method.

This allows to add a virtual column that refer to the parent of the created DBIx::Class row.

=head2 children

Returns a DBIx::Class Row of the parent of a node.

=head2 children

Returns a DBIx::Class resultset of all children (direct descendants) of a node.

=head2 leaf_children

Returns a DBIx::Class resultset of all children (direct descendants) of a node that do not possess children.

=head2 inner_children

Returns a DBIx::Class resultset of all children (direct descendants) of a node that possess children.

=head2 descendants

Returns a DBIx::Class resultset of all descendants of a node.

=head2 leaves

Returns a DBIx::Class resultset of all descendants of a node that do not possess children.

=head2 inner_descendants

Returns a DBIx::Class resultset of all descendants of a node that possess children.

=head2 ancestors

Returns a DBIx::Class resultset of all ancestors of a node.

=head2 ascendants

An alias method for ancestors.

=head2 root

Returns a DBIx::Class resultset containing the root node of a given node.

=head2 siblings

Returns a DBIx::Class resultset containing all the node with the same parent of a given node.

=head2 is_root

Returns 1 if the node has no parent, and 0 otherwise.

=head2 is_inner

Returns 1 if the node has children, and 0 otherwise.

=head2 is_branch

Returns 1 if the node has children and a parent, and 0 otherwise.

=head2 is_leaf

Returns 1 if the node has no child, and 0 otherwise.

=head2 available_mobius_index

Returns the smallest mobius index available in the subtree of a given node.

=head2 child_encoding

Given a mobius index, return the mobius a,b,c,d column values

 	
 	
=for Pod::Coverage new	mobius_path	root_cond inner_cond leaf_cond

