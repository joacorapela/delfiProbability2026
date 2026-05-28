import sys
import numpy as np
import scipy.stats
import plotly.graph_objects as go

def main(argv):
    n_samples = 10000
    mean_r = 10.0
    var_r = 2.0
    min_i = 6.0
    max_i = 10.0
    n_dense = 1000
    n_repeats = 1000
    fig_filename_pattern = "../../figures/fig_ex3.{:s}"

    analytical_var_w = 43497.96
    numerical_var_w = np.empty(n_repeats)
    for j in range(n_repeats):
        r = np.random.normal(loc=mean_r, scale=np.sqrt(var_r), size=n_samples)
        i = np.random.uniform(low=min_i, high=max_i, size=n_samples)
        w = r * i**2
        numerical_var_w[j] = np.std(w)**2
    median_numerical_var_we = np.median(numerical_var_w)

    fig = go.Figure()
    trace = go.Histogram(x=numerical_var_w, showlegend=False)
    fig.add_trace(trace)
    fig.add_vline(x=analytical_var_w, line_dash="solid")
    fig.add_vline(x=median_numerical_var_we, line_dash="dash")
    fig.update_xaxes(title="Varianza Muestral de W")
    fig.update_yaxes(title="Cuenta")
    fig.update_layout(title=f"Varianza analitica de W: {analytical_var_w:.2f}")
    fig.write_image(fig_filename_pattern.format("png"))
    fig.write_html(fig_filename_pattern.format("html"))

    print(f'figure saved to {fig_filename_pattern.format("html")}')
    fig.show()

    breakpoint()


if __name__ == "__main__":
    main(sys.argv)
