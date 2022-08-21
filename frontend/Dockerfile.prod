#base image
FROM node

RUN mkdir /install

#install dependencies global
RUN npm install -g react-scripts

ADD . /ft_transcendence-front
WORKDIR /ft_transcendence-front

#expose port
EXPOSE 3000

#start app
CMD ["bash", "prod.sh"]