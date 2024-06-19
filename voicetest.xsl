<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:output method="text"/>

    <xsl:template match="/">
        <xsl:apply-templates select="voice"/>
    </xsl:template>

    <xsl:template match="voice">
        <xsl:text>"</xsl:text>
        <xsl:value-of select="@name"/>
        <xsl:text>"</xsl:text>

        <xsl:apply-templates select="operators"/>
        <xsl:apply-templates select="peg"/>
        <xsl:apply-templates select="lfo"/>
    </xsl:template>

    <xsl:template match="operators">
        <xsl:apply-templates select="operator"/> <!-- TODO: iterate backwards -->
    </xsl:template>

    <xsl:template match="operator">
        <xsl:apply-templates select="eg"/>
    </xsl:template>

    <xsl:template match="*[operator]">
        <xsl:copy>
            <xsl:copy-of select="@*"/>
            <xsl:apply-templates>
                <xsl:sort select="position()"
                          data-type="number" order="descending"/>
            </xsl:apply-templates>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="eg">
        <xsl:apply-templates select="rates"/>
        <xsl:apply-templates select="levels"/>
    </xsl:template>

    <xsl:template match="peg">
        <xsl:apply-templates select="rates"/>
        <xsl:apply-templates select="levels"/>
    </xsl:template>

    <xsl:template match="rates">
        <xsl:apply-templates select="rate"/>
    </xsl:template>

    <xsl:template match="rate">
        <xsl:value-of select="."/> <!-- TODO: output in hex -->
        <xsl:text>&#160;</xsl:text>
    </xsl:template>

    <xsl:template match="levels">
        <xsl:apply-templates select="level"/>
    </xsl:template>

    <xsl:template match="level">
        <xsl:value-of select="."/> <!-- TODO: output in hex -->
        <xsl:text>&#160;</xsl:text>
    </xsl:template>

    <xsl:template match="levelscaling">
        <xsl:value-of select="@breakpoint" />
        <xsl:text>&#160;</xsl:text>
    </xsl:template>

    <xsl:template match="depth">
        <xsl:value-of select="@left" />
        <xsl:text>&#160;</xsl:text>
        <xsl:value-of select="@right" />
        <xsl:text>&#160;</xsl:text>
    </xsl:template>

    <xsl:template match="curve">
        <xsl:choose>
            <xsl:when test="@left = '+LIN'">
                <xsl:text>3</xsl:text>
            </xsl:when>
            <xsl:when test="@left = '-LIN'">
                <xsl:text>0</xsl:text>
            </xsl:when>
            <xsl:when test="@left = '-EXP'">
                <xsl:text>1</xsl:text>
            </xsl:when>
            <xsl:when test="@left = '+EXP'">
                <xsl:text>2</xsl:text>
            </xsl:when>
            <xsl:when test="@right = '+LIN'">
                <xsl:text>3</xsl:text>
            </xsl:when>
            <xsl:when test="@right = '-LIN'">
                <xsl:text>0</xsl:text>
            </xsl:when>
            <xsl:when test="@right = '-EXP'">
                <xsl:text>1</xsl:text>
            </xsl:when>
            <xsl:when test="@right = '+EXP'">
                <xsl:text>2</xsl:text>
            </xsl:when>
        </xsl:choose>
        <xsl:text>&#160;</xsl:text>
    </xsl:template>

</xsl:stylesheet>
