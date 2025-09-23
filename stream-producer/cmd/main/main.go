package main

import (
	"github.com/confluentinc/confluent-kafka-go/kafka"
	log "github.com/gothew/l-og"
)

func main() {
	//topic := "raw-blackholes"
	topic := "test-topic"
	p, err := kafka.NewProducer(&kafka.ConfigMap{
		"bootstrap.servers": "localhost:9092,localhost:9093,localhost:9094",
		"client.id":         "go-producer",
		"acks":              "all",
	})

	if err != nil {
		log.Errorf("Failed to create producer: %s", err)
	}

	defer p.Close()

	go func() {
		for e := range p.Events() {
			switch ev := e.(type) {
			case *kafka.Message:
				if ev.TopicPartition.Error != nil {
					log.Errorf("Failed to deliver message: %v\n", ev.TopicPartition)
				} else {
					log.Infof("Successfully produced record to topic %s partition [%d] @ offset %v\n",
						*ev.TopicPartition.Topic, ev.TopicPartition.Partition, ev.TopicPartition.Offset)
				}
			}
		}
	}()

	messageValue := "Hello, Kafka!"
	p.Produce(&kafka.Message{
		TopicPartition: kafka.TopicPartition{Topic: &topic, Partition: kafka.PartitionAny},
		Value:          []byte(messageValue),
	}, nil)
	p.Flush(15 * 1000)
}
