package net.danieldietrich.protectedregions.parser

import static net.danieldietrich.protectedregions.util.Strings.*

import java.util.List
import org.eclipse.xtend.lib.annotations.Accessors

abstract class TreeExtensions {

	/** Construct a Node. */
	def static <T> Node<T> Node(String id, Tree<T>... children) {
		val node = new Node<T>(id)
		children.forEach[node.add(it)]
		node
	}

	/** Construct a Leaf. */
	def static <T> Leaf(String id, T value) {
		new Leaf<T>(id, value)
	}
	
	/** Construct a Link. */
	def static <T, X extends Tree<T>> Link(X ref) {
		switch ref {
			Node<T> : new NodeLink<T>(ref)
			Leaf<T> : new LeafLink<T>(ref)
		}
	}
	
	/** Example: node.leafs.find('xxx') */
	def static <T, X extends Tree<T>> X find(Iterable<X> children, String _id) {
		children.findFirst[id.equals(_id)]
	}
	
	/** Return all children of type Leaf. */
	def static <T> Iterable<Leaf<T>> leafs(Node<T> node) {
		node.children.map[switch it {
			Leaf<T> : newArrayList(it)
			Node<T> : emptyList
		}].flatten
	}
	
	/** Return all children of type Node. */
	def static <T> Iterable<Node<T>> nodes(Node<T> node) {
		node.children.map[switch it {
			Node<T> : newArrayList(it)
			Leaf<T> : emptyList
		}].flatten
	}
	
	/**
	 * Traverses a Tree top down, applying a function f to each tree node.
	 * If f returns true, descend children, else go on with neighbors.
	 */
	def static <T> void traverse(Tree<T> tree, (Tree<T>)=>Boolean f) {
		val descend = f.apply(tree)
		if (descend === null || descend) switch tree {
			Node<T> : tree.children.forEach[traverse(it, f)]
		}
	}
	
}

abstract class Tree<T> {
	
	@Accessors val String id // identifier, not necessarily unique
	@Accessors var Node<T> parent = null
	
	new(String id) {
		if (id === null) throw new IllegalArgumentException("Id cannot be null")
		this.id = id
	}
	
	def isRoot() { parent === null }
	def Tree<T> root() { if (isRoot) this else parent.root() }
	
	override toString() { toString(0) }
	def String toString(int depth)
	
}

class Node<T> extends Tree<T> {
	
	@Accessors val List<Tree<T>> children = newArrayList()
	
	new(String id) {
		super(id)
	}
	
	def add(Tree<T> child) {
		children.add(child)
		child.parent = this
		child
	}
	
	override toString(int depth) {
		val indent = indent(depth)
		indent + id +"(\n"+ children.map[Tree<T> child | child.toString(depth+1)].reduce[l,r | l +",\n"+ r] +"\n"+ indent +")"
	}

}

class Leaf<T> extends Tree<T> {

	@Accessors val T value

	new(String id, T value) {
		super(id)
		this.value = value
	}
	
	override toString(int depth) {
		val indent = indent(depth)
		indent + id +"("+ value.toString.replaceAll("\\s+", " ") +")"
	}
	
}

class NodeLink<T> extends Node<T> {
	
	@Accessors Node<T> ref
	
	new(Node<T> ref) {
		super('Link->'+ ref.id)
		this.ref = ref
	}
	
	override getChildren() { ref.children }
	
	override add(Tree<T> child) {
		ref.add(child)
	}

	override toString(int depth) {
		val indent = indent(depth)
		indent + id
	}
	
}

class LeafLink<T> extends Leaf<T> {
	
	@Accessors Leaf<T> ref
	
	new(Leaf<T> ref) {
		super('Link->'+ ref.id, null)
		this.ref = ref
	}

	override getValue() { ref.value }

	override toString(int depth) {
		val indent = indent(depth)
		indent + id
	}
	
}
