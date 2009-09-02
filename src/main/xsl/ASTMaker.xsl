<?xml version="1.0" encoding="UTF-8"?>

<!--
    Document   : ASTMaker.xsl
    Created on : July 26, 2009, 11:41 AM
    Author     : andrew
    Description:
        Purpose of transformation follows.
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
    <xsl:output method="text"/>

    <!-- TODO customize transformation rules 
         syntax recommendation http://www.w3.org/TR/xslt 
    -->
    <xsl:template match="trait">
package <xsl:value-of select="@package"/>

import scala.util.parsing.input.{Position,NoPosition}

object <xsl:value-of select="@name"/> {
	type Identity = Int

	var uniqueId = 0
	def newId: Identity = {
		uniqueId += 1
		uniqueId
	}

	type PropertyMap = Map[Property[_], Any]
	def newIdProperty =	Node.Identity -> newId

	abstract class Node extends AstNode {
		def identity: Identity = apply(Node.Identity)
		def position: Position = get(Node.Position).getOrElse(NoPosition)
		override def toString = AstDescriber.describe(this)
	}
	object Node {
		object Identity extends Property[Identity]
		object Position extends Property[Position]
	}

<xsl:value-of select="preamble"/>
<xsl:apply-templates select="node"/>
}
    </xsl:template>

	<xsl:template match="node[@abstract='true']">
abstract class <xsl:value-of select="@name"/> extends <xsl:value-of select="@extends"/> {
	type thisType &lt;: <xsl:value-of select="@name"/>
<xsl:value-of select="node()"/>
}	</xsl:template>

	<xsl:template match="node">
class <xsl:value-of select="@name"/>(val properties: PropertyMap) extends <xsl:value-of select="@extends"/> {
	type thisType = <xsl:value-of select="@name"/>
	def factory = <xsl:value-of select="@name"/>
<xsl:apply-templates select="property" mode="property-defs"/>
<xsl:value-of select="."/><!-- Copy any custom code -->
}
object <xsl:value-of select="@name"/> extends NodeFactory[<xsl:value-of select="@name"/>] {
	def apply(props: PropertyMap) = new <xsl:value-of select="@name"/>(props)
	def apply(<xsl:apply-templates select="property" mode="params"/>) =
		new <xsl:value-of select="@name"/>(Map(newIdProperty<xsl:apply-templates select="property" mode="constructor-args"/>))
	def unapply(obj: <xsl:value-of select="@name"/>) = Some(<xsl:apply-templates select="property" mode="unapply"/>)
<xsl:apply-templates select="property" mode="property-object-defs"/>
}
	</xsl:template>

	<xsl:template match="property" mode="params">
		<xsl:variable name="actualtype">
			<xsl:choose>
				<xsl:when test="@type"><xsl:value-of select="@type"/></xsl:when>
				<xsl:when test="@nodeoptiontype"><xsl:value-of select="concat('Option[',@nodeoptiontype,']')"/></xsl:when>
				<xsl:when test="@nodetype"><xsl:value-of select="@nodetype"/></xsl:when>
				<xsl:when test="@nodelisttype"><xsl:value-of select="concat('Seq[',@nodelisttype,']')"/></xsl:when>
			</xsl:choose>
		</xsl:variable>
		<xsl:if test="position()>1">, </xsl:if><xsl:value-of select="@name"/>: <xsl:value-of select="$actualtype"/>
	</xsl:template>

	<xsl:template match="property" mode="property-defs">
		<xsl:variable name="upper" select="concat(upper-case(substring(@name, 1, 1)), substring(@name, 2))"/>
	def <xsl:value-of select="@name"/> = this(<xsl:value-of select="../@name"/>.<xsl:value-of select="$upper"/>)</xsl:template>

	<xsl:template match="property" mode="constructor-args"><xsl:variable name="upper" select="concat(upper-case(substring(@name, 1, 1)), substring(@name, 2))"
	/>, <xsl:value-of select="$upper"/>-&gt;<xsl:value-of select="@name"/></xsl:template>

	<xsl:template match="property" mode="unapply">
		<xsl:if test="position()>1">, </xsl:if>obj.<xsl:value-of select="@name"/>
	</xsl:template>

	<xsl:template match="property" mode="property-object-defs">
		<xsl:variable name="upper" select="concat(upper-case(substring(@name, 1, 1)), substring(@name, 2))"/>
	object <xsl:value-of select="$upper"/> extends <xsl:choose>
		<xsl:when test="@type">Property</xsl:when>
		<xsl:when test="@nodeoptiontype">Property0</xsl:when>
		<xsl:when test="@nodetype">Property1</xsl:when>
		<xsl:when test="@nodelisttype">PropertyN</xsl:when>
		</xsl:choose>[<xsl:value-of select="concat(@type,@nodeoptiontype,@nodetype,@nodelisttype)"/>]</xsl:template>

</xsl:stylesheet>
