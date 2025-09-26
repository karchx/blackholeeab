package main

import (
	"context"
	"fmt"
	"time"

	"github.com/confluentinc/confluent-kafka-go/kafka"
	log "github.com/gothew/l-og"

	"bytes"
	"os"
	"path/filepath"

	"github.com/tormoder/fit"
)

func readFitDatasets() {
	// Read our FIT test file data
	testFile := filepath.Join("example-fit.fits")
	log.Infof("Reading FIT file: %s", testFile)
	testData, err := os.ReadFile(testFile)
	if err != nil {
		fmt.Println(err)
		return
	}

	// Decode the FIT file data
	fit, err := fit.Decode(bytes.NewReader(testData))
	if err != nil {
		fmt.Println(err)
		return
	}

	// Inspect the TimeCreated field in the FileId message
	fmt.Println(fit.FileId.TimeCreated)

	// Inspect the dynamic Product field in the FileId message
	fmt.Println(fit.FileId.GetProduct())

	// Inspect the FIT file type
	fmt.Println(fit.Type())

	// Get the actual activity
	activity, err := fit.Activity()
	if err != nil {
		fmt.Println(err)
		return
	}

	// Print the latitude and longitude of the first Record message
	for _, record := range activity.Records {
		fmt.Println(record.PositionLat)
		fmt.Println(record.PositionLong)
		break
	}

	// Print the sport of the first Session message
	for _, session := range activity.Sessions {
		fmt.Println(session.Sport)
		break
	}

	// Output:
	// 2012-04-09 21:22:26 +0000 UTC
	// Hrm1
	// Activity
	// 41.51393
	// -73.14859
	// Running
}

// TODO: check config auto.create.topics.enable=true and delete.topics.enable=true
// on kafka service yaml
func ensureTopic(admin *kafka.AdminClient, topic string) error {
	meta, err := admin.GetMetadata(&topic, false, 5000)
	if err != nil {
		return fmt.Errorf("failed to get metadata: %v", err)
	}

	if _, exists := meta.Topics[topic]; exists {
		log.Infof("Topic %s already exists", topic)
		return nil
	}

	spec := []kafka.TopicSpecification{{
		Topic:             topic,
		NumPartitions:     3,
		ReplicationFactor: 3,
	}}

	results, err := admin.CreateTopics(context.TODO(), spec, kafka.SetAdminOperationTimeout(30*time.Second))
	if err != nil {
		return fmt.Errorf("failed to create topic: %v", err)
	}

	for _, result := range results {
		if result.Error.Code() != kafka.ErrNoError {
			return fmt.Errorf("failed to create topic %s: %v", result.Topic, result.Error)
		}
	}
	fmt.Printf("Topic %s created successfully\n", topic)
	return nil
}

func main() {
	readFitDatasets()
	//topic := "blackholes-raw"
	//servers := "localhost:9092,localhost:9093,localhost:9094"
	//admin, err := kafka.NewAdminClient(&kafka.ConfigMap{"bootstrap.servers": servers})

	//if err != nil {
	//	log.Errorf("Failed to create admin client: %s", err)
	//}
	//defer admin.Close()

	//if err := ensureTopic(admin, topic); err != nil {
	//	log.Errorf("Error ensuring topic: %v", err)
	//}

	//p, err := kafka.NewProducer(&kafka.ConfigMap{
	//	"bootstrap.servers": servers,
	//	"client.id":         "go-producer",
	//	"acks":              "all",
	//})

	//if err != nil {
	//	log.Errorf("Failed to create producer: %s", err)
	//}

	//defer p.Close()

	//go func() {
	//	for e := range p.Events() {
	//		switch ev := e.(type) {
	//		case *kafka.Message:
	//			if ev.TopicPartition.Error != nil {
	//				log.Errorf("Failed to deliver message: %v\n", ev.TopicPartition)
	//			} else {
	//				log.Infof("Successfully produced record to topic %s partition [%d] @ offset %v\n",
	//					*ev.TopicPartition.Topic, ev.TopicPartition.Partition, ev.TopicPartition.Offset)
	//			}
	//		}
	//	}
	//}()

	//messageValue := "Hello, Kafka!"
	//p.Produce(&kafka.Message{
	//	TopicPartition: kafka.TopicPartition{Topic: &topic, Partition: kafka.PartitionAny},
	//	Value:          []byte(messageValue),
	//}, nil)
	//p.Flush(15 * 1000)
}
