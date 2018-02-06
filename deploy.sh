#!/bin/bash
# A sample Bash script, by Ryan

sbt assembly
tar -czvf chat-bot.tar target/scala-2.12/chat-bot-server-assembly-0.1.jar target/scala-2.12/link
scp -i ~/Downloads/tradebot.pem  chat-bot.tar ec2-user@ec2-54-244-181-157.us-west-2.compute.amazonaws.com:~/
