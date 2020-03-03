PRIV := ../../../priv

all: \
    $(PRIV)/critical.rgba \
    $(PRIV)/idea.rgba \
    $(PRIV)/info.rgba \
    $(PRIV)/warning.rgba

$(PRIV)/%.rgba: %.png
	convert -define h:format=rgba -depth 8 $< $@
