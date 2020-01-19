-module(display).
-export([start/0, draw_image/5, clear_screen/2, draw_rect/6, draw_pixel/4, draw_text/5]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-record(state, {
    spi = undefined,
    gpio = undefined
}).

draw_image(Display, X, Y, Image, BackgroundColor) ->
    avm_gen_server:call(Display, {draw_image, X, Y, Image, BackgroundColor}, 60000).

clear_screen(Display, Color) ->
    avm_gen_server:call(Display, {clear_screen, Color}, 60000).

draw_rect(Display, X, Y, Width, Height, Color) ->
    avm_gen_server:call(Display, {draw_rect, X, Y, Width, Height, Color}, 60000).

draw_pixel(Display, X, Y, Color) ->
    avm_gen_server:call(Display, {draw_pixel, X, Y, Color}, 60000).

draw_text(Display, X, Y, Text, Color) ->
    avm_gen_server:call(Display, {draw_text, X, Y, Text, Color}, 60000).

start() ->
    case atomvm:platform() of
        generic_unix ->
            Display = open_port({spawn, "display"}, [{height, 240}, {width, 320}]),
            {ok, Display};

        esp32 ->
            {ok, P} = avm_gen_server:start(?MODULE, [], []),
            avm_gen_server:call(P, init, 10000),
            {ok, P}
    end.

init(_) ->
    {ok, #state{}}.

handle_call(init, _From, #state{}) ->
    {SPI, GPIO} = lcd_init(),
    {reply, ok, #state{spi=SPI, gpio=GPIO}};

handle_call({clear_screen, Color}, _From, #state{spi=SPI, gpio=GPIO} = State) ->
    clear_screen(SPI, GPIO, Color),
    {reply, ok, State};

handle_call({draw_image, X, Y, {W, H, Pixels}, BackgroundColor}, _From, #state{spi=SPI, gpio=GPIO} = State) ->
    draw_image(SPI, GPIO, X, Y, W, H, Pixels, BackgroundColor),
    {reply, ok, State};

handle_call({draw_rect, X, Y, Width, Height, Color}, _From, #state{spi=SPI, gpio=GPIO} = State) ->
    draw_rect(SPI, GPIO, X, Y, Width, Height, Color),
    {reply, ok, State};

handle_call({draw_pixel, X, Y, Color}, _From, #state{spi=SPI, gpio=GPIO} = State) ->
    draw_pixel(SPI, GPIO, X, Y, Color),
    {reply, ok, State};

handle_call({draw_text, X, Y, Text, Color}, _From, #state{spi=SPI, gpio=GPIO} = State) ->
    draw_text(SPI, GPIO, X, Y, Text, Color),
    {reply, ok, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

-define (ILI9341_SLPIN, 16#10).
-define (ILI9341_SLPOUT, 16#11).
-define (ILI9341_PTLON, 16#12).
-define (ILI9341_NORON, 16#13).

-define (ILI9341_INVOFF, 16#20).
-define (ILI9341_INVON, 16#21).
-define (ILI9341_GAMMASET, 16#26).
-define (ILI9341_DISPOFF, 16#28).
-define (ILI9341_DISPON, 16#29).


-define (ILI9341_PTLAR, 16#30).
-define (ILI9341_VSCRDEF, 16#33).
-define (ILI9341_MADCTL, 16#36).
-define (ILI9341_VSCRSADD, 16#37).
-define (ILI9341_PIXFMT, 16#3A).

-define (ILI9341_FRMCTR1, 16#B1).
-define (ILI9341_FRMCTR2, 16#B2).
-define (ILI9341_FRMCTR3, 16#B3).
-define (ILI9341_INVCTR, 16#B4).
-define (ILI9341_DFUNCTR, 16#B6).

-define (ILI9341_PWCTR1, 16#C0).
-define (ILI9341_PWCTR2, 16#C1).
-define (ILI9341_PWCTR3, 16#C2).
-define (ILI9341_PWCTR4, 16#C3).
-define (ILI9341_PWCTR5, 16#C4).
-define (ILI9341_VMCTR1, 16#C5).
-define (ILI9341_VMCTR2, 16#C7).

-define (ILI9341_GMCTRP1, 16#E0).
-define (ILI9341_GMCTRN1, 16#E1).

-define (TFT_SWRST, 16#01).
-define (TFT_CASET, 16#2A).
-define (TFT_PASET, 16#2B).
-define (TFT_RAMWR, 16#2C).

-define (TFT_MADCTL, 16#36).
-define (TFT_MAD_MY, 16#80).
-define (TFT_MAD_MX, 16#40).
-define (TFT_MAD_MV, 16#20).
-define (TFT_MAD_BGR, 16#08).

-define (SET_LEVEL, set_level).
-define (WRITE_AT, write_at).

-define (SPISettings, [
    {bus_config, [
            {miso_io_num, 19},
            {mosi_io_num, 23},
            {sclk_io_num, 18}
        ]},
        {device_config, [
            {spi_clock_hz, 27000000},
            {spi_mode, 0},
            {spi_cs_io_num, 5},
            {address_len_bits, 0}
        ]}
    ]).

lcd_init() ->
    GPIO = gpio:open(),

    lcd_init(GPIO, ?SPISettings).

lcd_init(GPIO, SPISettings) ->
    gpio:set_direction(GPIO, 2, output),
    gpio:set_level(GPIO, 2, 1),

    SPI = spi:open(SPISettings),

    gpio:set_direction(GPIO, 21, output),

    writecommand(SPI, GPIO, ?TFT_SWRST),

    avm_timer:sleep(5),

    %%% INIT START
    writecommand(SPI, GPIO, 16#EF),
    writedata(SPI, 16#03),
    writedata(SPI, 16#80),
    writedata(SPI, 16#02),

    writecommand(SPI, GPIO, 16#CF),
    writedata(SPI, 16#00),
    writedata(SPI, 16#C1),
    writedata(SPI, 16#30),

    writecommand(SPI, GPIO, 16#ED),
    writedata(SPI, 16#64),
    writedata(SPI, 16#03),
    writedata(SPI, 16#12),
    writedata(SPI, 16#81),

    writecommand(SPI, GPIO, 16#E8),
    writedata(SPI, 16#85),
    writedata(SPI, 16#00),
    writedata(SPI, 16#78),

    writecommand(SPI, GPIO, 16#CB),
    writedata(SPI, 16#39),
    writedata(SPI, 16#2C),
    writedata(SPI, 16#00),
    writedata(SPI, 16#34),
    writedata(SPI, 16#02),

    writecommand(SPI, GPIO, 16#F7),
    writedata(SPI, 16#20),

    writecommand(SPI, GPIO, 16#EA),
    writedata(SPI, 16#00),
    writedata(SPI, 16#00),

    writecommand(SPI, GPIO, ?ILI9341_PWCTR1),
    writedata(SPI, 16#23),

    writecommand(SPI, GPIO, ?ILI9341_PWCTR2),
    writedata(SPI, 16#10),

    writecommand(SPI, GPIO, ?ILI9341_VMCTR1),
    writedata(SPI, 16#3e),
    writedata(SPI, 16#28),

    writecommand(SPI, GPIO, ?ILI9341_VMCTR2),
    writedata(SPI, 16#86),

    writecommand(SPI, GPIO, ?ILI9341_MADCTL),
    writedata(SPI, 16#08),

    writecommand(SPI, GPIO, ?ILI9341_PIXFMT),
    writedata(SPI, 16#55),

    writecommand(SPI, GPIO, ?ILI9341_FRMCTR1),
    writedata(SPI, 16#00),
    writedata(SPI, 16#13),

    writecommand(SPI, GPIO, ?ILI9341_DFUNCTR),
    writedata(SPI, 16#0A),
    writedata(SPI, 16#A2),
    writedata(SPI, 16#27),

    writecommand(SPI, GPIO, 16#F2),
    writedata(SPI, 16#00),

    writecommand(SPI, GPIO, ?ILI9341_GAMMASET),
    writedata(SPI, 16#01),

    writecommand(SPI, GPIO, ?ILI9341_GMCTRP1),
    writedata(SPI, 16#0F),
    writedata(SPI, 16#31),
    writedata(SPI, 16#2B),
    writedata(SPI, 16#0C),
    writedata(SPI, 16#0E),
    writedata(SPI, 16#08),
    writedata(SPI, 16#4E),
    writedata(SPI, 16#F1),
    writedata(SPI, 16#37),
    writedata(SPI, 16#07),
    writedata(SPI, 16#10),
    writedata(SPI, 16#03),
    writedata(SPI, 16#0E),
    writedata(SPI, 16#09),
    writedata(SPI, 16#00),

    writecommand(SPI, GPIO, ?ILI9341_GMCTRN1),
    writedata(SPI, 16#00),
    writedata(SPI, 16#0E),
    writedata(SPI, 16#14),
    writedata(SPI, 16#03),
    writedata(SPI, 16#11),
    writedata(SPI, 16#07),
    writedata(SPI, 16#31),
    writedata(SPI, 16#C1),
    writedata(SPI, 16#48),
    writedata(SPI, 16#08),
    writedata(SPI, 16#0F),
    writedata(SPI, 16#0C),
    writedata(SPI, 16#31),
    writedata(SPI, 16#36),
    writedata(SPI, 16#0F),

    writecommand(SPI, GPIO, ?ILI9341_SLPOUT),

    avm_timer:sleep(120),

    writecommand(SPI, GPIO, ?ILI9341_DISPON),

    set_rotation(SPI, GPIO, 1),

    {SPI, GPIO}.

draw_text(_SPI, _GPIO, _X, _Y, [], _Color) ->
    ok;

draw_text(SPI, GPIO, X, Y, [H | T], Color) ->
    draw_font(SPI, GPIO, H, X, Y, Color),
    draw_text(SPI, GPIO, X + 8, Y, T, Color).

draw_font(SPI, GPIO, Char, X, Y, Color) ->
    Glyph = get_glyph(Char),
    draw_font(SPI, GPIO, Glyph, 0, X, Y, Color).

draw_font(_SPI, _GPIO, _Font, 8 * 16, _X, _Y, _Color) ->
    ok;

draw_font(SPI, GPIO, Font, Index, X, Y, Color) ->
    Int = erlang:element((Index div 8) + 1, Font),
    draw_font_pixel(SPI, GPIO, Int, Index rem 8, X + (Index rem 8), Y + (Index div 8), Color),
    draw_font(SPI, GPIO, Font, Index + 1, X, Y, Color).

draw_font_pixel(SPI, GPIO, Int, Index, X, Y, Color) ->
    if
        Int band (1 bsl (7 - Index)) /= 0 -> draw_pixel(SPI, GPIO, X, Y, Color);
        true -> ok
    end.

set_rotation(SPI, GPIO, 0) ->
    writecommand(SPI, GPIO, ?TFT_MADCTL),
    writedata(SPI, ?TFT_MAD_BGR);

set_rotation(SPI, GPIO, 1) ->
    writecommand(SPI, GPIO, ?TFT_MADCTL),
    writedata(SPI, ?TFT_MAD_BGR bor ?TFT_MAD_MY bor ?TFT_MAD_MV).

draw_pixel(SPI, GPIO, X, Y, Color) ->
    writecommand(SPI, GPIO, ?TFT_CASET),
    spi:?WRITE_AT(SPI, 0, 32, (X bsl 16) bor X),

    writecommand(SPI, GPIO, ?TFT_PASET),
    spi:?WRITE_AT(SPI, 0, 32, (Y bsl 16) bor Y),

    writecommand(SPI, GPIO, ?TFT_RAMWR),
    spi:?WRITE_AT(SPI, 0, 16, Color).

clear_screen(SPI, GPIO, Color) ->
    draw_rect(SPI, GPIO, 0, 0, 320, 240, Color).

draw_rect(SPI, GPIO, X, Y, Length, Height, Color) ->
    writecommand(SPI, GPIO, ?TFT_CASET),
    spi:write_at(SPI, 0, 32, (X bsl 16) bor (X + Length - 1)),

    writecommand(SPI, GPIO, ?TFT_PASET),
    spi:write_at(SPI, 0, 32, (Y bsl 16) bor (Y + Height - 1)),

    writecommand(SPI, GPIO, ?TFT_RAMWR),
    put_n_pixels(SPI, Length * Height, Color).

draw_image(SPI, GPIO, X, Y, W, H, Pixels, BackgroundColor) ->
    writecommand(SPI, GPIO, ?TFT_CASET),
    spi:write_at(SPI, 0, 32, (X bsl 16) bor (X + W - 1)),

    writecommand(SPI, GPIO, ?TFT_PASET),
    spi:write_at(SPI, 0, 32, (Y bsl 16) bor (Y + H - 1)),

    writecommand(SPI, GPIO, ?TFT_RAMWR),
    put_image_pixels(SPI, 0, Pixels, W * H * 4, BackgroundColor).

put_image_pixels(_SPI, Num, _Pixels, PixelsCount, _BackgroundColor) when Num >= PixelsCount ->
    ok;

put_image_pixels(SPI, Num, Pixels, PixelsCount, BackgroundColor) ->
    A = binary:at(Pixels, Num + 3),
    if
        A > 0 ->
            R = binary:at(Pixels, Num) bsr 3,
            G = binary:at(Pixels, Num + 1) bsr 2,
            B = binary:at(Pixels, Num + 2) bsr 3,
            spi:?WRITE_AT(SPI, 0, 16, (R bsl 11) bor (G bsl 5) bor B);

        true ->
            spi:?WRITE_AT(SPI, 0, 16, BackgroundColor)
    end,
    put_image_pixels(SPI, Num + 4, Pixels, PixelsCount, BackgroundColor).

put_n_pixels(_SPI, 0, _Color) ->
    ok;

put_n_pixels(SPI, N, Color) ->
    spi:?WRITE_AT(SPI, 0, 16, Color),
    put_n_pixels(SPI, N - 1, Color).

writecommand(SPI, GPIO, C) ->
    gpio:?SET_LEVEL(GPIO, 21, 0),
    spi:?WRITE_AT(SPI, 0, 8, C),
    gpio:?SET_LEVEL(GPIO, 21, 1).

writedata(SPI, D) ->
    spi:?WRITE_AT(SPI, 0, 8, D).

get_glyph(Char) ->
    case Char of
       $\s -> { 16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00};
       _Any -> { 16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#FF,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00,16#00}
    end.
