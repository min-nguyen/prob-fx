import sys
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import ast
from sklearn import linear_model
from scipy.special import expit
import numpy as np
from scipy.interpolate import make_interp_spline

def save_multi_image(filename):
   pp = PdfPages(filename)
   fig_nums = plt.get_fignums()
   figs = [plt.figure(n) for n in fig_nums]
   for fig in figs:
      fig.savefig(pp, format='pdf')
   pp.close()

def main():
  arg  = sys.argv[1]
  f    = open("model-output.txt", "r")

  data = ast.literal_eval(f.read().replace('-Infinity', '-2e308')) #
  color_map = plt.cm.get_cmap('Blues')
  if arg == "simLinRegr":
    xys =  [[ i for i, j in data ],
            [ j for i, j in data ]]
    xs = xys[0]
    ys = xys[1]
    plt.scatter(xs, ys)
    plt.xlabel('x data points')
    plt.ylabel('y data points')
    plt.title('Linear regression')

  if arg == "lwLinRegr":
    mus = [d[0] for d in data]
    ps  = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('mu value')
    axs1.set_ylabel('probability')
    axs1.scatter(mus, ps)
    axs1.set_title('Linear regression - Likelihood Weighting')

  if arg == "mhLinRegr":
    mus = data
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values", fontsize=12)
    axs1.set_ylabel("frequency")
    axs1.hist(mus, bins=25)
    axs1.set_title('Linear regression - Metropolis Hastings')

  if arg == "simSIR" or arg == "simSIRS":
    # y axis
    sir_values   = np.array(data[0])
    obs_infected = np.array(data[1])
    sus            = np.array([sir[0] for sir in sir_values])
    inf            = np.array([sir[1] for sir in sir_values])
    recov          = np.array([sir[2] for sir in sir_values])
    # x axis
    timeSteps      = np.array([ t for t in range(len(sus))])
    # interpolate data
    X_ = np.linspace(timeSteps.min(), timeSteps.max(), 300)
    X_S_Spline = make_interp_spline(timeSteps.ravel(), sus.ravel())
    X_I_Spline = make_interp_spline(timeSteps.ravel(), inf.ravel())
    X_R_Spline = make_interp_spline(timeSteps.ravel(), recov.ravel())
    X_InfCount_Spline = make_interp_spline(timeSteps.ravel(), obs_infected.ravel())
    S_ = X_S_Spline(X_)
    I_ = X_I_Spline(X_)
    R_ = X_R_Spline(X_)
    IC_ = X_InfCount_Spline(X_)

    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("days")
    axs1.set_ylabel("population", fontsize=12)
    axs1.plot(X_, S_, color='blue', label='Actual Susceptible')
    axs1.plot(X_, I_, color='red', label='Actual Infected')
    axs1.plot(X_, R_, color='green', label='Actual Recovered')
    axs1.plot(X_, IC_, color='black', label='Reported Infected')
    axs1.set_title('SIR model - Simulation')
    plt.xlim([0,100])
    plt.ylim([0,800])
    plt.legend()

  if arg == "simSIRSV":
    # y axis
    sirv_values   = np.array(data[0])
    obs_infected = np.array(data[1])
    sus            = np.array([sirv[0] for sirv in sirv_values])
    inf            = np.array([sirv[1] for sirv in sirv_values])
    recov          = np.array([sirv[2] for sirv in sirv_values])
    vacc          = np.array([sirv[3] for sirv in sirv_values])
    # x axis
    timeSteps      = np.array([ t for t in range(len(sus))])
    # interpolate data
    X_ = np.linspace(timeSteps.min(), timeSteps.max(), 300)
    X_S_Spline = make_interp_spline(timeSteps.ravel(), sus.ravel())
    X_I_Spline = make_interp_spline(timeSteps.ravel(), inf.ravel())
    X_R_Spline = make_interp_spline(timeSteps.ravel(), recov.ravel())
    X_V_Spline = make_interp_spline(timeSteps.ravel(), vacc.ravel())
    X_InfCount_Spline = make_interp_spline(timeSteps.ravel(), obs_infected.ravel())
    S_ = X_S_Spline(X_)
    I_ = X_I_Spline(X_)
    R_ = X_R_Spline(X_)
    V_ = X_V_Spline(X_)
    IC_ = X_InfCount_Spline(X_)

    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("days")
    axs1.set_ylabel("population", fontsize=12)
    axs1.plot(X_, S_, color='blue', label='Actual Susceptible')
    axs1.plot(X_, I_, color='red', label='Actual Infected')
    axs1.plot(X_, R_, color='green', label='Actual Recovered')
    axs1.plot(X_, V_, color='orange', label='Actual Vaccinated')
    axs1.plot(X_, IC_, color='black', label='Reported Infected')
    axs1.set_title('SIRSV model - Simulation')
    plt.xlim([0,100])
    plt.ylim([0,800])
    plt.legend()

  if arg == "mhSIR":
    rhos_unique   = data[0]
    betas_unique  = data[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("ρ values", fontsize=12)
    axs1.set_ylabel("frequency")
    axs1.hist(rhos_unique, bins=55)
    axs1.set_title('SIR - Metropolis Hastings Posterior (Rho)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("β values", fontsize=12)
    axs2.set_ylabel("frequency")
    axs2.hist(betas_unique, bins=55)
    axs2.set_title('HMM - Metropolis Hastings Posterior (Beta)')

  if arg == "simLogRegr":
    xys = np.array(data)
    xs =  np.array([xy[0] for xy in xys])
    ys =  np.array([xy[1] for xy in xys])
    model = linear_model.LogisticRegression(C=1e5, solver='lbfgs')
    model.fit(xs.reshape(-1,1), ys)
    x_test = np.linspace(-2.0,2.0,num=100)
    y_test = x_test * model.coef_ + model.intercept_
    sigmoid = expit(y_test)
    plt.yticks([1.0, 0.0], ["True",
                            "False"])
    plt.scatter(xs, ys)
    plt.plot(x_test, sigmoid.ravel(),c="green", label = "logistic fit")
    plt.xlabel('x - axis')
    plt.ylabel('y - axis')
    plt.title('Logistic regression simulation')

  if arg == "lwLogRegr":
    mus = [d[0] for d in data]
    ps  = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel('mu value')
    axs1.set_ylabel('probability')
    axs1.scatter(mus, ps)
    axs1.set_title('Logistic regression - Likelihood Weighting')

  if arg == "mhLogRegr":
    mu_samples = [d[0] for d in data]
    b_samples  = [d[1] for d in data]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_samples, bins=50)
    axs1.set_title('Logistic regression - Metropolis Hastings Posterior')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("b values")
    axs2.set_ylabel("frequency")
    axs2.hist(b_samples, bins=50)
    axs2.set_title('Logistic regression - Metropolis Hastings Posterior')

  if arg == "simLDA":
    print(data)
    words = list(np.array(data).ravel())
    _, ax = plt.subplots(nrows=1)
    ws = list(set(words))
    freqs = [ words.count(w)  for w in ws]
    ax.bar(ws, freqs)
    ax.set_xticklabels(ws)

  if arg == "mhLDA":
    ws          = ['DNA', 'evolution', 'parsing', 'phonology']
    topic_ps = data[0][0]
    topic_0s = data[1][0]
    topic_1s = data[1][1]
    fig, ax = plt.subplots(nrows=1)
    ax.bar(['Topic 0', 'Topic 1'], topic_ps, 0.8)
    ax.set_xticklabels(['Topic 0', 'Topic 1'])
    plt.title('Document-Topic Distribution')
    fig0, ax0 = plt.subplots(nrows=1)
    ax0.bar(ws, topic_0s, 0.8)
    ax0.set_xticklabels(ws)
    plt.title('Topic-Word Distribution 0')
    fig1, ax1 = plt.subplots(nrows=1)
    ax1.bar(ws, topic_1s, 0.8)
    ax1.set_xticklabels(ws)
    plt.title('Topic-Word Distribution 1')

  if arg == "simRadon":
    basement_ys   = data[0]
    basement_xs   = [0 for i in range(len(basement_ys))]
    nobasement_ys = data[1]
    nobasement_xs = [1 for i in range(len(nobasement_ys))]
    print(data)
    plt.scatter(basement_xs, basement_ys, color="r")
    plt.scatter(nobasement_xs, nobasement_ys, color='b')
    plt.ylabel('Log radon level')
    plt.xticks([0, 1], ["basement", "no basement"])

  if arg == "mhRadon":
    intercepts = data[0]
    gradients  = data[1]
    plt.xticks([0, 1], ["basement", "no basement"])
    plt.ylabel('Log radon level')
    for (m, c) in zip(gradients, intercepts):
      x = np.linspace(0, 1, 100)
      y = m * x + c
      plt.plot(x, y)

  if arg == "mhRadonpost":
    mu_a       = data[0]
    mu_b       = data[1]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu_a values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_a, bins=50)
    axs1.set_title('HLR - Metropolis Hastings Posterior (mu_a)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("mu_b values")
    axs2.set_ylabel("frequency")
    axs2.hist(mu_b, bins=50)
    axs2.set_title('HLR - Metropolis Hastings Posterior (mu_b)')

  if arg == "mhSchool":
    mu_samples_unique   = data[0]
    thetas              = data[1]
    thetas_             = [[d[i] for d in thetas] for i in range(len(thetas[0]))]
    fig1, axs1 = plt.subplots(nrows=1)
    axs1.set_xlabel("mu values")
    axs1.set_ylabel("frequency")
    axs1.hist(mu_samples_unique, bins=50)
    axs1.set_title('School - Metropolis Hastings Posterior (mu)')
    fig2, axs2 = plt.subplots(nrows=1)
    axs2.set_xlabel("theta")
    axs2.set_ylabel("value")
    axs2.boxplot(thetas_)
    axs2.set_title('School - Metropolis Hastings Posterior (thetas)')

  save_multi_image("model-output.pdf")
  plt.show()
if __name__ == "__main__":
  main()
