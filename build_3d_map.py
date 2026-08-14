import pandas as pd
import pydeck as pdk

df = pd.read_csv("nova_datacenters.csv")

df["color"] = df["status"].apply(lambda s: [0, 180, 166, 200] if s == "existing" else [255, 140, 66, 200])
df["elevation"] = (df["mw"] ** 0.5) * 700

layer = pdk.Layer(
    "ColumnLayer",
    data=df,
    get_position=["lon", "lat"],
    get_elevation="elevation",
    elevation_scale=1,
    radius=450,
    get_fill_color="color",
    pickable=True,
    auto_highlight=True,
)

# Position each label at its column's actual top in 3D space (lon, lat, elevation)
# Bigger, bolder text with a white outline so it stays legible at wide zoom levels.
text_layer = pdk.Layer(
    "TextLayer",
    data=df,
    get_position=["lon", "lat", "elevation"],
    get_text="name",
    get_size=22,
    get_color=[10, 10, 10, 255],
    get_angle=0,
    get_text_anchor="'middle'",
    get_alignment_baseline="'bottom'",
    get_pixel_offset=[0, -8],
    font_family="'Helvetica Neue', Arial, sans-serif",
    font_weight=700,
    outline_width=4,
    outline_color=[255, 255, 255, 255],
    billboard=True,
)

tooltip = {
    "html": "<b>{name}</b><br/>"
            "Operator: {operator}<br/>"
            "City: {city}, {county} County<br/>"
            "Status: {status}<br/>"
            "~{sqft} sq ft / {mw} MW",
    "style": {"backgroundColor": "steelblue", "color": "white"}
}

r = pdk.Deck(
    layers=[layer, text_layer],
    initial_view_state=pdk.ViewState(
        latitude=df["lat"].mean(),
        longitude=df["lon"].mean(),
        zoom=10.5,
        pitch=35,
        bearing=0,
    ),
    tooltip=tooltip,
    map_provider="carto",
    map_style="light",
)

r.to_html("nova_datacenters_3d.html", open_browser=False)
print("Saved nova_datacenters_3d.html")

import plotly.express as px

fig = px.scatter_mapbox(
    df,
    lat="lat", lon="lon",
    size="mw",
    color="status",
    text="name",
    hover_name="name",
    hover_data={"operator": True, "city": True, "county": True, "sqft": True, "mw": True, "lat": False, "lon": False},
    zoom=9,
    mapbox_style="open-street-map"
)
fig.update_traces(textposition="top center", textfont=dict(size=11, color="black"))
fig.update_layout(margin=dict(l=0, r=0, t=0, b=0), height=1000, width=1600)
fig.write_image("nova_datacenters_map.png", scale=2)
print("Saved nova_datacenters_map.png")
Add 3D map build script

Add 3D map build script
